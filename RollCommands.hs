{-# LANGUAGE OverloadedStrings #-}
module RollCommands (
  rollNWOD,
  rollGeneric,
  rollGURPS,
  rollEval,
  rollDnd
) where

import qualified Network.IRC as IRCB
import qualified Network.IRC.Commands as IRCC
import qualified Data.ByteString as BS
import Control.Monad
import Control.Monad.Except
import Data.Maybe(fromMaybe,fromJust,isNothing,isJust)
import Control.Monad.Trans.Class
import qualified Data.ByteString.Char8 as C

import qualified Config
import qualified Database
import Common
import Connection(write,sendMsg,sendNotice)
import Data.List(intercalate)
import System.Process(readProcessWithExitCode)
import System.Exit(ExitCode(..))
import Control.Monad.IO.Class(liftIO)

import Control.Applicative ((<*>),
                            (*>),
                            (<$>),
                            (<|>),
                            pure)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import System.Random
import System.IO.Unsafe
import Prelude hiding (takeWhile)

rollDnd :: PrivMsg -> String -> BotAction ()
rollDnd pmsg args = do
  gen <- getBotRandom (minBound :: Int) (maxBound :: Int)
  case parseDnd args gen of
    Left err -> sendMsg (getResponseTarget pmsg) ("Error: " ++ err)
    Right (rolls, result) -> sendMsg (getResponseTarget pmsg) $ foldl1 (++) [source, " rolls ", rolls, ". Result: ", result]
  where
    source = getSourceNick . getSource $ pmsg

parseDnd :: String -> Int ->  Either String (String, String)
parseDnd s gen = parseOnly (parseDndExpr gen) (B.pack s)

parseDndExpr :: Int -> Parser (String, String)
parseDndExpr gen = do
  ns <- parseNList
  bonus <- (skipSpace *> signed decimal) <|> (skipSpace >> endOfInput >> return 0)
  let rolls = Prelude.take (length ns) (infiniteRoll gen)
  let result = map (+bonus) $ zipWith (+) rolls ns
  return (show rolls, show result)
  where
    parseNList :: Parser [Int]
    parseNList = do
      n <- signed decimal
      (char '/' *> fmap (n:) parseNList) <|> (return [n])

attackRoll :: [Int] -> Int -> String
attackRoll mods gen = foldl1 (++) [show roll, " -> ", show res]
  where
    roll = Prelude.take (length mods) (infiniteRoll gen)
    res = zipWith (+) mods roll

attackRollB mods bonus = attackRoll $ map (+bonus) mods

infiniteRoll :: Int -> [Int]
infiniteRoll gen = rec (mkStdGen gen)
  where
    rec g = r : rec g'
      where (r, g') = randomR (1, 20) g

  

rollEval :: PrivMsg -> String -> BotAction ()
rollEval pmsg args = do
  case parseRd args of
    Left err -> return ()
    Right (sShow, sCode) -> do
      let msg o e = foldl1 (++) [source, " rolls ", sShow, ". Result: ", o, e]
      result <- liftIO $ readProcessWithExitCode Config.muevalBinary ["+RTS", "-N", "-RTS", "-l", "MuevalInclude.hs", "--expression", sCode] "" 
      case result of
        (ExitFailure 127, _, _) -> sendMsg (getResponseTarget pmsg) "failed to run mueval"
        (ExitFailure 1, o, e) -> sendMsg (getResponseTarget pmsg) (msg o e)
        (ExitSuccess, o, e) -> sendMsg (getResponseTarget pmsg) (msg o e)
        (_, _, _) -> sendMsg (getResponseTarget pmsg) "failure occurred"
  where 
    source = getSourceNick . getSource $ pmsg

parseRd :: String -> Either String (String, String)
parseRd s = parseOnly parseRdExpr (B.pack s)

data RollState a = NoRoll a | SumRoll a | NoSumRoll a | End
parseRdExpr :: Parser (String, String)
parseRdExpr = do
  p <- parseRoll <|>
       (takeWhile1 (not . isDigit) >>= return . NoRoll . B.unpack) <|> 
       (takeWhile1 isDigit >>= return . NoRoll . B.unpack) <|>
       (endOfInput >> return End)
  case p of
    NoRoll s -> do
      (sShow, sCode) <- parseRdExpr
      return (s ++ sShow, s ++ sCode)
    SumRoll rolls -> do
      (sShow, sCode) <- parseRdExpr
      return (rolls ++ sShow, "(sum " ++ rolls ++ ")" ++ sCode)
    NoSumRoll rolls -> do
      (sShow, sCode) <- parseRdExpr
      return (rolls ++ sShow, rolls ++ sCode)
    End -> return ("", "")
  where
    parseRoll = do
      n <- fmap (\n -> if n > 1000 then 0 else n) decimal
      rollType <- (char 'd' >> return SumRoll) <|> (char 'r' >> return NoSumRoll) --purpose of 5r20 is to not output (sum [<rolls>]) later
      d <- decimal
      --This makes me feel dirty, but attoparsec doesn't like monad transformers, and parsec makes for more complicated code.
      return . rollType . show $ (unsafePerformIO $ replicateM (fromIntegral n) $ randomRIO (1, d) :: [Integer])


rollGeneric :: PrivMsg -> String -> BotAction ()
rollGeneric pmsg args = do
  parse <- runExceptT $ do
    let (dice, sides) = break (=='d') args
    let diceNum = readInt dice
    let sidesNum = readInt (tail sides)
    when (isNothing diceNum || null sides || isNothing sidesNum) $ throwError "Syntax must be like <n>d<m>."
    let Just (diceNumInt, rem1) = diceNum
    let Just (sidesNumInt, rem2) = sidesNum
    when (length rem1 == 1 || length rem2 == 1) $ throwError "Non-numeric characters found."
    when (diceNumInt > 100) $ throwError "Max of 100 dice."
    when (diceNumInt < 1 || sidesNumInt < 1) $ throwError "Cannot have zero or negative dice or sides."
    return (diceNumInt, sidesNumInt)
        
  case parse of
    Left e -> sendMsg (getResponseTarget pmsg) $ "Error: " ++ e
    Right (numDice, numSides) -> do
      rolls <- replicateM numDice $ getBotRandom 1 numSides
      sendMsg (getResponseTarget pmsg) $ foldl1 (++) [getSourceNick . getSource $ pmsg, " rolls ", args, ". Result: ", show rolls, " (", show . sum $ rolls, ")"]

rollGURPS :: PrivMsg -> String -> BotAction ()
rollGURPS pmsg args = do
	let maybeTarget = readInt args
	if isNothing maybeTarget then do
		sendMsg responseTarget $ "Invalid GURPS roll syntax. Must be like =rg 15"
	else do
		let Just (target, _) = maybeTarget
		result <- foldl1 (+) `fmap` (replicateM 3 $ getBotRandom 1 6)
		let successOrFailure = let margin = show (target - result) in case result of
			1 -> "\ETX03CRITICAL SUCCESS\ETX, margin " ++ margin
			18 -> "\ETX04CRITICAL FAILURE\ETX"
			_ -> if result <= target then "\ETX03SUCCESS\ETX, margin " ++ margin else "\ETX04FAILURE\ETX, margin " ++ margin
		sendMsg responseTarget $ foldl1 (++) [source, " rolls ", show result, " against ", show target, " (", successOrFailure, ")" ]
	where
		responseTarget = getResponseTarget pmsg
		source = getSourceNick . getSource $ pmsg


data BatchRollResult = Batch [Int] BatchRollResult

rollNWOD :: PrivMsg -> String -> BotAction ()
rollNWOD pmsg args = do
	(resultBatch,successTotal) <- rollNWODDiceBatch (max 1 count)
	let resultDescription = renderBatch resultBatch
	sendMsg responseTarget $ foldl1 (++) ([source, " rolls ", dieDescription, ". Result: ",
		resultDescription, " ("] ++ 
			case successTotal of
				0 -> ["\ETX04failure\ETX)"]
				1 -> ["\ETX031 success\ETX)"]
				otherwise -> ["\ETX03",show successTotal, " successes\ETX)" ])
	where
		responseTarget = getResponseTarget pmsg
		source = getSourceNick . getSource $ pmsg
		(countUnlimited, remainingStr) = fromMaybe (0,"") (readInt args)
		count = max 0 (min 100 countUnlimited)
		rollAgain = 
			if (not . null) remainingStr && head remainingStr == 'r' && count > 0 then 
				let (rollAgain, _) = fromMaybe (10,"") (readInt (tail remainingStr)) in max 5 rollAgain
			else 
				10
		successThreshold = if count == 0 then 10 else 8
		dieDescription = 
			if count == 0 then 
				"\ETX04a chance die\ETX" 
			else 
				foldl1 (++) [show count, " dice with ", show rollAgain, "-again"]
		rollNWODDiceBatch remaining = 
			if remaining <= 0 then 
				return (Batch [] undefined,0)
			else do
				batchResults <- replicateM remaining (getBotRandom 1 10)
				let successes = length . filter (\roll -> roll >= successThreshold) $ batchResults
				let nextBatchSize = length . filter (\roll -> roll >= rollAgain) $ batchResults
				(nextBatch,nextBatchSuccesses) <- rollNWODDiceBatch nextBatchSize
				return $ (Batch batchResults nextBatch, successes + nextBatchSuccesses)
		renderBatch (Batch rolls next) = rollsString ++ " " ++ renderNextBatch next
			where
				rollsString = intercalate ", " . map (\r -> if r >= successThreshold then "\ETX03" ++ show r ++ "\ETX" else show r) $ rolls
				renderNextBatch b@(Batch r _) = 
					if null r then 
						""
					else
						"{ " ++ renderBatch b ++ "}"


