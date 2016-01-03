{-# LANGUAGE OverloadedStrings #-}

module Block.Parse 
  ( Block (Block)
  , parseBlocks
  ) where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString)
import Control.Applicative ((<|>))

data Block = Block String String [(String, String)] deriving Show

parseBlocks :: ByteString -> Either String [Block]
parseBlocks str = parseOnly ((many' parseBlock) <* endOfInput) str

parseBlock :: Parser Block
parseBlock = do
  name        <- parseAttrWithKey "NAME"
  type_       <- parseAttrWithKey "TYPE"
  attrs       <- manyTill parseAttr (string "END")
  _           <- endOfLine
  return (Block name type_ attrs)

parseAttr :: Parser (String, String)
parseAttr = do
  _     <- delim
  key   <- manyTill anyChar (char ' ' <|> char '\t')
  _     <- delim >> char '=' >> delim
  value <- manyTill anyChar endOfLine
  return (key, value)

parseAttrWithKey :: ByteString -> Parser String
parseAttrWithKey key = do
  _     <- delim
  _     <- string key
  _     <- delim >> char '=' >> delim
  value <- manyTill anyChar endOfLine
  return value

delim :: Parser String
delim = many' (char ' ' <|> char '\t')
