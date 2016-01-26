{-# LANGUAGE OverloadedStrings #-}

module Block.Parse 
  ( Block (Block)
  , parseBlocks
  ) where

import Data.Attoparsec.ByteString as A (takeTill)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString, unpack)

data Block = Block String String [(String, String)] deriving Show

parseBlocks :: ByteString -> Either String [Block]
parseBlocks str = parseOnly ((many' parseBlock) <* endOfInput) str

parseBlock :: Parser Block
parseBlock = do
  name        <- parseAttrWithKey "NAME"
  type_       <- parseAttrWithKey "TYPE"
  attrs       <- manyTill parseAttr (string "END")
  _           <- endOfLine
  return (Block (unpack name) (unpack type_)
                (map (\ (a, b) -> (unpack a, unpack b)) attrs))

parseAttr :: Parser (ByteString, ByteString)
parseAttr = do
  key   <- skipSpace *> A.takeTill isHorizontalSpace <* skipSpace
  _     <- char '='
  value <- skipSpace *> A.takeTill isEndOfLine <* endOfLine
  return (key, value)

parseAttrWithKey :: ByteString -> Parser ByteString
parseAttrWithKey key = skipSpace *> string key *> skipSpace *> char '='
                    *> skipSpace *> A.takeTill isEndOfLine <* endOfLine
