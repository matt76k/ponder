import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.Char
import Text.Ponder

numChar = oneOf ['0'..'9']

nNumStr :: Parser String
nNumStr = do a <- numChar
	     if a == '0'
	     	then 
		  do notP $ numChar
		     return "0"
	       	else 
		  do b <- many $ numChar
	       	     return (a:b)

dNumStr :: Parser String
dNumStr = do a <- nNumStr <|> string "0"
	     b <- string "."
	     c <- some $ numChar
             return (a ++ b ++ c)

numStr :: Parser String
numStr = (++) <$> (string "-" <|> pure []) <*> (dNumStr <|> nNumStr) -- optional?

num :: Parser Double
num = numStr >>= \x -> StateT $ \xs-> return (read x, xs) 


calc :: Parser Double
calc = expr <* notP item

expr :: Parser Double
expr = do t <- term
       	  do s <- item
	     case s of 
	       '+' -> do e <- expr
	       	      	 return (t + e) 
	       '-' -> do e <- expr
	       	      	 return (t - e)
	       _   ->  StateT $ \_-> mzero
             <|> return t

term :: Parser Double
term = do f <- factor
       	  do s <- item
	     case s of 
	       '*' -> do t <- term
	       	      	 return (f * t) 
	       '/' -> do t <- term
	       	      	 return (f / t) 
--	       _   -> StateT $ \_-> mzero -- should not use StateT
             <|> return f

factor :: Parser Double
factor = char '(' *> expr <* char ')' <|> num
