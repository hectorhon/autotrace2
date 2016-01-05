{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module TimeSeriesData.Expressions
  ( parseExpression
  , evaluate
  , listTags
  ) where

import Prelude hiding (and, or)
import Text.Parsec
import Control.Monad.Reader
import TimeSeriesData.Types
import TimeSeriesData.Compares
import TimeSeriesData.Arithmetic
import TimeSeriesData.Counts

data Expression = TknTagName String
                | TknNumber Double
                | TknString String
                | TknData [TSPoint]
                | TknInvalid
                | TknAdd Expression Expression
                | TknMinus Expression Expression
                | TknMoreThan Expression Expression
                | TknMoreEqThan Expression Expression
                | TknLessThan Expression Expression
                | TknLessEqThan Expression Expression
                | TknEquals Expression Expression
                | TknNotEquals Expression Expression
                | TknAnd Expression Expression
                | TknOr Expression Expression
                | TknIntervals [TSInterval] deriving Show

parseExpression :: String -> Either ParseError Expression
parseExpression = parse expr "Invalid expression"

expr :: forall s u (m :: * -> *) . Stream s m Char
     => ParsecT s u m Expression
expr = do e <- logical
          eof
          return e

logical :: forall s u (m :: * -> *) . Stream s m Char
        => ParsecT s u m Expression
logical = chainl1 (logical' <|> comparison) op
  where op = (string "and" <|> string "AND" <|> string "or" <|> string "OR")
             >>= \ sym -> case sym of "and" -> return TknAnd
                                      "AND" -> return TknAnd
                                      "or"  -> return TknOr
                                      "OR"  -> return TknOr
                                      _     -> undefined

logical' :: forall s u (m :: * -> *) . Stream s m Char
         => ParsecT s u m Expression
logical' = do _ <- try $ spaces >> char '(' >> spaces
              a <- try logical
              _ <- try $ spaces >> char ')' >> spaces
              return a


comparison :: forall s u (m :: * -> *) . Stream s m Char
           => ParsecT s u m Expression
comparison = do t1 <- term
                sym <-     try (string ">=")
                       <|> try (string "<=")
                       <|> try (string "!=")
                       <|> try (string ">")
                       <|> try (string "<")
                       <|> try (string "=")
                t2 <- term
                let op = case sym of ">"  -> TknMoreThan
                                     ">=" -> TknMoreEqThan
                                     "<"  -> TknLessThan
                                     "<=" -> TknLessEqThan
                                     "="  -> TknEquals
                                     "!=" -> TknNotEquals
                                     _    -> undefined
                return (op t1 t2)

-- |A term represents a collection of values
term :: forall s u (m :: * -> *) . Stream s m Char
     => ParsecT s u m Expression
term = do spaces
          t <- try arith <|> tagName <|> num <|> str
          spaces
          return t

arith :: forall s u (m :: * -> *) . Stream s m Char
           => ParsecT s u m Expression
arith = chainl1 (tagName <|> num) op
  where op = do sym <- oneOf "+-"
                return $ case sym of '+' -> TknAdd
                                     '-' -> TknMinus
                                     _   -> undefined

tagName :: forall s u (m :: * -> *) . Stream s m Char
        => ParsecT s u m Expression
tagName = between (char '[') (char ']') (many1 $ alphaNum <|> oneOf "._")
          >>= return . TknTagName

num :: forall s u (m :: * -> *) . Stream s m Char
    => ParsecT s u m Expression
num = integer <++> decimal <++> expo >>= return . TknNumber . read
      where integer = (positive <|> negative <|> nums)
            positive = char '+' *> nums
            negative = (:) <$> char '-' <*> nums
            nums = many1 digit
            decimal = option "" $ char '.' <:> nums
            expo = option "" $ oneOf "eE" <:> integer
            (<++>) a b = (++) <$> a <*> b
            (<:>) a b = (:) <$> a <*> b

str :: forall s u (m :: * -> *) . Stream s m Char
    => ParsecT s u m Expression
str = between (char '"') (char '"') (many letter) >>= return . TknString

listTags :: Expression -> [String]
listTags (TknMoreThan c1 c2) = concat [listTags c1, listTags c2]
listTags (TknMoreEqThan c1 c2) = concat [listTags c1, listTags c2]
listTags (TknLessThan c1 c2) = concat [listTags c1, listTags c2]
listTags (TknLessEqThan c1 c2) = concat [listTags c1, listTags c2]
listTags (TknEquals c1 c2) = concat [listTags c1, listTags c2]
listTags (TknNotEquals c1 c2) = concat [listTags c1, listTags c2]
listTags (TknAdd c1 c2) = concat [listTags c1, listTags c2]
listTags (TknMinus c1 c2) = concat [listTags c1, listTags c2]
listTags (TknAnd c1 c2) = concat [listTags c1, listTags c2]
listTags (TknOr c1 c2) = concat [listTags c1, listTags c2]
listTags (TknTagName tn) = [tn]
listTags _ = []

evaluate :: Expression -> TSData -> [TSInterval]
evaluate expression tsData = case runReader (eval expression) tsData of
  TknIntervals intervals -> intervals
  _                      -> []



eval :: MonadReader TSData m => Expression -> m Expression

eval (TknTagName tn) = reader (lookup tn . dataOf)
                       >>= return . maybe TknInvalid TknData

eval (TknNumber n) = do start <- reader startOf
                        end   <- reader endOf
                        let v = Continuous n
                        return $ TknData $ [TSPoint start v, TSPoint end v]

eval (TknString n) = do start <- reader startOf
                        end   <- reader endOf
                        let v = Discrete n
                        return $ TknData $ [TSPoint start v, TSPoint end v]

eval (TknData points) = return $ TknData points

eval TknInvalid = return TknInvalid

eval (TknAdd (TknData d1) (TknData d2)) = return $
  TknData $ arithmetic (+) d1 d2

eval (TknMinus (TknData d1) (TknData d2)) = return $
  TknData $ arithmetic (-) d1 d2

eval (TknMoreThan (TknData d1) (TknData d2)) = return $ TknIntervals $
  fmap fst $ filter ((== (Just GT)) . snd) (compares d1 d2)

eval (TknMoreEqThan (TknData d1) (TknData d2)) = return $ TknIntervals $
  fmap fst $ filter f (compares d1 d2)
  where f (_, a) = a == (Just GT) || a == (Just EQ)

eval (TknLessThan (TknData d1) (TknData d2)) = return $ TknIntervals $
  fmap fst $ filter ((== (Just LT)) . snd) (compares d1 d2)

eval (TknLessEqThan (TknData d1) (TknData d2)) = return $ TknIntervals $
  fmap fst $ filter f (compares d1 d2)
  where f (_, a) = a == (Just LT) || a == (Just EQ)

eval (TknEquals (TknData d1) (TknData d2)) = return $ TknIntervals $
  fmap fst $ filter ((== (Just EQ)) . snd) (compares d1 d2)

eval (TknNotEquals (TknData d1) (TknData d2)) = return $ TknIntervals $
  fmap fst $ filter f (compares d1 d2)
  where f (_, a) = a == (Just LT) || a == (Just GT)

eval (TknAnd (TknIntervals s1) (TknIntervals s2)) =
  return $ TknIntervals $ map fst $ filter ((== 2) . snd) (counts [s1, s2])

eval (TknOr (TknIntervals s1) (TknIntervals s2)) =
  return $ TknIntervals $ map fst $ filter ((> 0) . snd) (counts [s1, s2])

eval (TknIntervals intervals) = return $ TknIntervals intervals

eval (TknAdd        TknInvalid _) = return TknInvalid
eval (TknMinus      TknInvalid _) = return TknInvalid
eval (TknMoreThan   TknInvalid _) = return TknInvalid
eval (TknMoreEqThan TknInvalid _) = return TknInvalid
eval (TknLessThan   TknInvalid _) = return TknInvalid
eval (TknLessEqThan TknInvalid _) = return TknInvalid
eval (TknEquals     TknInvalid _) = return TknInvalid
eval (TknNotEquals  TknInvalid _) = return TknInvalid
eval (TknAnd        TknInvalid _) = return TknInvalid
eval (TknOr         TknInvalid _) = return TknInvalid

eval (TknAdd        _ TknInvalid) = return TknInvalid
eval (TknMinus      _ TknInvalid) = return TknInvalid
eval (TknMoreThan   _ TknInvalid) = return TknInvalid
eval (TknMoreEqThan _ TknInvalid) = return TknInvalid
eval (TknLessThan   _ TknInvalid) = return TknInvalid
eval (TknLessEqThan _ TknInvalid) = return TknInvalid
eval (TknEquals     _ TknInvalid) = return TknInvalid
eval (TknNotEquals  _ TknInvalid) = return TknInvalid
eval (TknAnd        _ TknInvalid) = return TknInvalid
eval (TknOr         _ TknInvalid) = return TknInvalid

eval (TknAdd c1 c2) = do c1' <- eval c1
                         c2' <- eval c2
                         eval $ TknAdd c1' c2'
eval (TknMinus c1 c2) = do c1' <- eval c1
                           c2' <- eval c2
                           eval $ TknMinus c1' c2'
eval (TknMoreThan c1 c2) = do c1' <- eval c1
                              c2' <- eval c2
                              eval $ TknMoreThan c1' c2'
eval (TknMoreEqThan c1 c2) = do c1' <- eval c1
                                c2' <- eval c2
                                eval $ TknMoreEqThan c1' c2'
eval (TknLessThan c1 c2) = do c1' <- eval c1
                              c2' <- eval c2
                              eval $ TknLessThan c1' c2'
eval (TknLessEqThan c1 c2) = do c1' <- eval c1
                                c2' <- eval c2
                                eval $ TknLessEqThan c1' c2'
eval (TknEquals c1 c2) = do c1' <- eval c1
                            c2' <- eval c2
                            eval $ TknEquals c1' c2'
eval (TknNotEquals c1 c2) = do c1' <- eval c1
                               c2' <- eval c2
                               eval $ TknNotEquals c1' c2'
eval (TknAnd c1 c2) = do c1' <- eval c1
                         c2' <- eval c2
                         eval $ TknAnd c1' c2'
eval (TknOr c1 c2) = do c1' <- eval c1
                        c2' <- eval c2
                        eval $ TknOr c1' c2'
