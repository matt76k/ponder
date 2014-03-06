import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.Char
import Text.Ponder

-- 一桁の数字
numChar = oneOf ['0'..'9']

-- 正の整数の文字列
nNumStr :: Parser String
nNumStr = do a <- numChar
	     if a == '0'
	     	then 
		  do notP $ numChar
		     return "0"
	       	else 
		  do b <- many $ numChar
	       	     return (a:b)

-- 正の小数の文字列
dNumStr :: Parser String
dNumStr = do a <- nNumStr <|> string "0"
	     b <- string "."
	     c <- some $ numChar
             return (a ++ b ++ c)

-- 実数の文字列
numStr :: Parser String
numStr = (++) <$> (string "-" <|> pure []) <*> (dNumStr <|> nNumStr)

-- 数字列(String)を数(Double)に変換するパーサー
num :: Parser Double
num = numStr >>= \x -> StateT $ \xs-> return (read x,xs) 


-- 四則演算パーサー、()付き四則演算を行う
-- 数式(String)を計算結果(Double)に加工
calc :: Parser Double
calc = do e <- expr
       	  notP item
	  return e

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
	       _   -> StateT $ \_-> mzero
             <|> return f

factor :: Parser Double
factor = do char '('
       	    e <- expr
	    char ')'
	    return e 
	 <|> num
