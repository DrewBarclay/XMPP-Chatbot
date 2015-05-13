{-# LANGUAGE OverloadedStrings #-}

module XmlUtils (
  unwrapMessage,
  wrapMessage,
  boldNode,
  boldText,
  italicsNode,
  italicsText,
  newline,
  text,
  nodesToString
) where

import Data.XML.Types
import Data.Text (unpack, pack, intercalate, toLower)
import Prelude hiding (intercalate)

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
wrapMessage :: [Node] -> [Element]
wrapMessage ns = [body, htmlBody]
  where 
    body = Element {elementName = "body", elementAttributes = [], elementNodes = [NodeContent . ContentText . nodesToText $ ns]}
    htmlBody = Element {elementName = "{http://jabber.org/protocol/xhtml-im}html", elementAttributes = [], elementNodes = [NodeElement $ Element {elementName = "{http://www.w3.org/1999/xhtml}body", elementAttributes = [], elementNodes=ns}]}

nodeToText (NodeElement e) = nodesToText $ elementNodes e
nodeToText (NodeContent (ContentText t)) = t
nodeToText _ = ""
nodesToText ns = intercalate "" $ map nodeToText ns

nodesToString = unpack . nodesToText

--Generate a <strong> node and give it a children
boldNode :: [Node] -> Node
boldNode ns = NodeElement $ Element {elementName = "strong", elementAttributes = [], elementNodes = ns}

--Generate <strong>text</strong> node
boldText :: String -> Node
boldText s = boldNode [text s]

--Generate an <em> node and give it children
italicsNode :: [Node] -> Node
italicsNode ns = NodeElement $ Element {elementName = "em", elementAttributes = [], elementNodes = ns}

--Generate <em>text</em> node
italicsText :: String -> Node
italicsText s = italicsNode [text s]

--Generates a newline (\n + <br />)
newline :: Node
newline = NodeElement $ Element {elementName = "br", elementAttributes = [], elementNodes = [text "\n"]}

--Generate text node
text :: String -> Node
text s = NodeContent $ ContentText (pack s)
