{-# LANGUAGE OverloadedStrings #-}

module Block.Parse 
  ( Block (Block)
  , parseBlocks
  ) where

import Data.Attoparsec.ByteString (takeTill)
import Data.Attoparsec.ByteString.Char8 hiding (takeTill, parse, eitherResult)
import Data.Attoparsec.ByteString.Lazy (parse, eitherResult)
import qualified Data.ByteString.Lazy as L (ByteString)
import Data.ByteString.Char8 (ByteString, unpack)

data Block = Block String String [(String, String)] deriving Show

parseBlocks :: L.ByteString -> Either String [Block]
parseBlocks = eitherResult . parse ((many' parseBlock) <* endOfInput)

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
  key   <- skipSpace *> takeTill isHorizontalSpace <* skipSpace
  _     <- char '='
  value <- skipSpace *> takeTill isEndOfLine <* endOfLine
  return (key, value)

parseAttrWithKey :: ByteString -> Parser ByteString
parseAttrWithKey key = skipSpace *> string key *> skipSpace *> char '='
                    *> skipSpace *> takeTill isEndOfLine <* endOfLine
