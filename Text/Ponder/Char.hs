module Text.Ponder.Char
( Parser
, parse
, item
, satisfy
, char
, string
, oneOf
) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity
import Control.Applicative

import Text.Ponder.Prim
import Text.Ponder.Pos

type Parser a = ParserT String String Identity a
parse p s = runIdentity $ runStateT (runErrorT . runStateT p $ s) (initialPos "")

item :: Parser Char
item = StateT $ \s -> case s of
                        c:cs -> return (c, cs)
                        otherwise -> mzero

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do c <- item
               if f c then return c
                      else mzero

char :: Char -> Parser Char
char c = satisfy (c==)

string :: String -> Parser String
string []     = return []
string (c:cs) = (:) <$> (char c) <*> (string cs)

oneOf :: String -> Parser Char
oneOf cs = satisfy (\c -> elem c cs)
