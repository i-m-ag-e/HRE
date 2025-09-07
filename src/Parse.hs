-- {-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Parse where

import           AST
import           Control.Applicative
import Data.Functor (($>))

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

runParser :: Parser a -> String -> Maybe a 
runParser (Parser p) =  fmap fst . p

instance Functor Parser where
    f `fmap` (Parser p) = Parser $ \s ->
        case p s of
            Nothing       -> Nothing
            Just (v, out) -> Just (f v, out)

instance Applicative Parser where
    pure v = Parser $ \s -> Just (v, s)
    (Parser p1) <*> p2 = Parser $ \s ->
        case p1 s of
            Nothing        -> Nothing
            Just (f, rest) -> (f <$> p2) `parse` rest

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \s ->
        case p1 s of
            Nothing  -> p2 s
            Just res -> Just res

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \s ->
        case p s of
            Nothing        -> Nothing
            Just (v, rest) -> parse (f v) rest

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s ->
    case s of
        (x:xs) | p x -> Just (x, xs)
        _            -> Nothing

peek :: Parser Char
peek = Parser $ \s ->
    if null s
        then Nothing
        else Just (head s, s)

isNotEmpty :: Parser ()
isNotEmpty = peek $> ()

char :: Char -> Parser Char
char c = satisfy (== c)

charIn :: [Char] -> Parser Char
charIn cs = satisfy (`elem` cs)

isMetaChar :: Char -> Bool
isMetaChar c = c `elem` "|*+()\\?."

emptyOr :: Parser RExpr -> Parser RExpr
emptyOr p = (isNotEmpty *> p) <|> return Empty

parseRE :: Parser RExpr
parseRE = emptyOr $ do
    union <- parseUnionTerm
    (char '|' *> (Union union <$> parseRE))
        <|> return union

parseUnionTerm :: Parser RExpr
parseUnionTerm = do
    conc <- parseConcatTerm
    (Concat conc <$> parseUnionTerm) <|> return conc

getRepeatType :: Parser (RExpr -> RExpr)
getRepeatType = (char '*' $> Star) <|> (char '+' $> Plus) <|> (char '?' $> Option) <|> return id

parseConcatTerm :: Parser RExpr
parseConcatTerm = do 
    term <- parseTerm 
    rep <- getRepeatType
    return (rep term)

parseTerm :: Parser RExpr
parseTerm =   (char '.' $> Wildcard)
          <|> (char '\\' *> (Symbol <$> satisfy isMetaChar))
          <|> (Symbol <$> satisfy (not . isMetaChar))
          <|> (char '(' *> parseRE <* char ')')

