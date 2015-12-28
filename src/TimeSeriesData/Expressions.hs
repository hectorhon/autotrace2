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
expr = do e <- choice [try and, try or, try clause]
          eof
          return e

and :: forall s u (m :: * -> *) . Stream s m Char
    => ParsecT s u m Expression
and = do c1 <- clause
         _  <- (string "and") <|> (string "AND")
         c2 <- choice [try and, try or, try clause]
         return $ TknAnd c1 c2

or :: forall s u (m :: * -> *) . Stream s m Char
   => ParsecT s u m Expression
or = do c1 <- clause
        _  <- (string "or") <|> (string "OR")
        c2 <- choice [try and, try or, try clause]
        return $ TknOr c1 c2



-- |A clause represents a collection of bools
clause :: forall s u (m :: * -> *) . Stream s m Char
       => ParsecT s u m Expression
clause = choice [ try moreThan, try moreEqThan
                , try lessThan, try lessEqThan
                , try equals  , try notEquals  ]

moreThan :: forall s u (m :: * -> *) . Stream s m Char
         => ParsecT s u m Expression
moreThan = do t1 <- term'
              _  <- char '>'
              t2 <- term'
              return $ TknMoreThan t1 t2

moreEqThan :: forall s u (m :: * -> *) . Stream s m Char
           => ParsecT s u m Expression
moreEqThan = do t1 <- term'
                _  <- string ">="
                t2 <- term'
                return $ TknMoreEqThan t1 t2

lessThan :: forall s u (m :: * -> *) . Stream s m Char
         => ParsecT s u m Expression
lessThan = do t1 <- term'
              _  <- char '<'
              t2 <- term'
              return $ TknLessThan t1 t2

lessEqThan :: forall s u (m :: * -> *) . Stream s m Char
           => ParsecT s u m Expression
lessEqThan = do t1 <- term'
                _  <- string "<="
                t2 <- term'
                return $ TknLessEqThan t1 t2

equals :: forall s u (m :: * -> *) . Stream s m Char
       => ParsecT s u m Expression
equals = do t1 <- term'
            _  <- char '='
            t2 <- term'
            return $ TknEquals t1 t2

notEquals :: forall s u (m :: * -> *) . Stream s m Char
          => ParsecT s u m Expression
notEquals = do t1 <- term'
               _  <- string "!="
               t2 <- term'
               return $ TknNotEquals t1 t2



-- |A term' represents a collection of values, processed for arithmetic
term' :: forall s u (m :: * -> *) . Stream s m Char
      => ParsecT s u m Expression
term' = do spaces
           t <- choice [ try add, try minus, try self ]
           spaces
           return t

add :: forall s u (m :: * -> *) . Stream s m Char
    => ParsecT s u m Expression
add = do t1 <- choice [ try tagName, try num ]
         _  <- char '+'
         t2 <- choice [ try tagName, try num ]
         return $ TknAdd t1 t2

minus :: forall s u (m :: * -> *) . Stream s m Char
      => ParsecT s u m Expression
minus = do t1 <- choice [ try tagName, try num ]
           _  <- char '-'
           t2 <- choice [ try tagName, try num ]
           return $ TknMinus t1 t2

self :: forall s u (m :: * -> *) . Stream s m Char
     => ParsecT s u m Expression
self = term



-- |A term represents a collection of values
term :: forall s u (m :: * -> *) . Stream s m Char
     => ParsecT s u m Expression
term = do spaces
          t <- choice [ try tagName, try num, try str ]
          spaces
          return t

tagName :: forall s u (m :: * -> *) . Stream s m Char
        => ParsecT s u m Expression
tagName = do _  <- char '['
             tn <- many1 (alphaNum <|> oneOf "._")
             _  <- char ']'
             return $ TknTagName tn

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
str = do _ <- char '"'
         s <- many letter
         _ <- char '"'
         return $ TknString s

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
