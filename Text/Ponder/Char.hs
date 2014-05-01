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

parse :: Parser a -> String -> Either String ((a, String), SourcePos)
parse p s = runIdentity $ evalStateT (runErrorT $ runStateT (runStateT p s) $ initialPos "") []

item :: Parser Char
item = StateT $ \s -> case s of c:cs -> do p <- get
                                           put $ updatePosChar p
                                           return (c, cs)
                                otherwise -> throwError "end of input"

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do c <- item
               if f c then StateT $ \s -> return (c,s)
                      else StateT $ \s -> do p <- get
                                             put $ backPosChar p
                                             throwError "not match"

char :: Char -> Parser Char
char c = satisfy (c==)

string :: String -> Parser String
string []     = return []
string (c:cs) = (:) <$> (char c) <*> (string cs)

oneOf :: String -> Parser Char
oneOf cs = satisfy (\c -> elem c cs)
