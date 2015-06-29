{-# LANGUAGE OverloadedStrings #-}

module XmlUtils (
  unwrapMessage,
  wrapMessage,
  boldNode,
  boldText,
  italicsNode,
  italicsText,
  attribute,
  emptyName,
  bareName,
  newline,
  text,
  nodesToString,
  mapNodeText,
  mapElementText
) where

import Data.XML.Types
import Data.Text (unpack, pack, intercalate, toLower, Text, replace)
import Prelude hiding (intercalate)
import Data.Text.Lazy.Builder (fromText, toLazyText)
import Data.Text.Lazy (toStrict)
import Data.Monoid (mconcat)

--Based on sample XML sent from Pidgin: [Element {elementName = Name {nameLocalName = "active", nameNamespace = Just "http://jabber.org/protocol/chatstates", namePrefix = Nothing}, elementAttributes = [], elementNodes = []},Element {elementName = Name {nameLocalName = "body", nameNamespace = Just "jabber:client", namePrefix = Nothing}, elementAttributes = [], elementNodes = [NodeContent (ContentText "test")]}]

--HTML XML: <message from='moogle@thegoodsirs.net/774002734143179800787955' to='bot2@thegoodsirs.net' type='chat' id='purple688c8bdf'><active xmlns='http://jabber.org/protocol/chatstates'/><body>italics</body><html xmlns='http://jabber.org/protocol/xhtml-im'><body xmlns='http://www.w3.org/1999/xhtml'><p><em>italics</em></p></body></html></message>

--Take the payload of a message, and return the html body if possible; the regular body if not; and Nothing if it is not possible.
unwrapMessage :: [Element] -> Maybe [Node]
unwrapMessage msg
  | hasHtml = Just htmlBody
  | hasBody = Just body
  | otherwise = Nothing
  where
    htmlList = filter (\e -> toLower (nameLocalName . elementName $ e) == "html") msg
    htmlBodyList = filter htmlBodyFilter $ elementNodes (head htmlList)
    htmlBodyFilter (NodeElement e) = toLower (nameLocalName . elementName $ e) == "body"
    htmlBodyFilter _ = False
    htmlBody = nodeChildren $ head htmlBodyList
    hasHtml = not (null htmlList) && not (null htmlBodyList)

    bodyList = filter (\e -> toLower (nameLocalName . elementName $ e) == "body") msg
    hasBody = not $ null bodyList
    body = elementNodes $ head bodyList

--Take a message, generates plain text <body> and html <body> [Element] for payload.
--Current hacky workaround for Pidgin: < and > are replaced with the suceeds/precedes unicode symbols in plaintext body.
wrapMessage :: [Node] -> [Element]
wrapMessage ns = [body, htmlBody]
  where 
    body = Element {elementName = "body", elementAttributes = [], elementNodes = [NodeContent . ContentText . toStrict . toLazyText $ nodesToText ns]}
    htmlBody = Element {elementName = "{http://jabber.org/protocol/xhtml-im}html", elementAttributes = [], elementNodes = [NodeElement $ Element {elementName = "{http://www.w3.org/1999/xhtml}body", elementAttributes = [], elementNodes=ns}]}

nodeToText (NodeElement e) = nodesToText $ elementNodes e
nodeToText (NodeContent (ContentText t)) = fromText t
nodeToText _ = fromText ""
nodesToText ns = mconcat $ map nodeToText ns

nodesToString = unpack . toStrict . toLazyText . nodesToText

mapNodeText :: (Text -> Text) -> Node -> Node
mapNodeText f (NodeElement e) = NodeElement (mapElementText f e)
mapNodeText f (NodeContent (ContentText t)) = NodeContent (ContentText $ f t)
mapNodeText _ n = n

mapElementText f e = e {elementNodes=map (mapNodeText f) (elementNodes e)}

emptyName :: Name
emptyName = Name {nameLocalName = "", nameNamespace = Just "http://www.w3.org/1999/xhtml", namePrefix = Nothing}

bareName :: Text -> Name
bareName name = emptyName {nameLocalName = name}

attribute :: Text -> Text -> (Name, [Content])
attribute name value = (emptyName {nameLocalName = name, nameNamespace = Nothing}, [ContentText value])

--Generate a <strong> node and give it a children
boldNode :: [Node] -> Node
boldNode ns = NodeElement $ Element {elementName = bareName "span", elementAttributes = [attribute "style" "font-weight: bold"], elementNodes = ns}

--Generate <strong>text</strong> node
boldText :: String -> Node
boldText s = boldNode [text s]

--Generate an <em> node and give it children
italicsNode :: [Node] -> Node
italicsNode ns = NodeElement $ Element {elementName = bareName "em", elementAttributes = [], elementNodes = ns}

--Generate <em>text</em> node
italicsText :: String -> Node
italicsText s = italicsNode [text s]

--Generates a newline (\n + <br />)
newline :: Node
newline = NodeElement $ Element {elementName = bareName "br", elementAttributes = [], elementNodes = [text "\n"]}

--Generate text node
text :: String -> Node
text s = NodeContent $ ContentText (pack s)
