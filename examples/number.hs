import Control.Monad
import Control.Applicative ( (<$>), (<*>), (<*) )
import Text.Ponder

{- PEG
digit  = (1/2/3/4/5/6/7/8/9)
digit0 = (0/digit)

zero   = (0,digit0!)

num    = (zero/-?pNum)
pNum   = (dNum/nNum,!.)
dNum   = ((nNum/zero),.,dPart)
dPart  = ((digit0,dPart)/(digit+,!0))
nNum   = (digit,digit0*)
-} 

-- 非零数字,数字
digit = oneOf ['1'..'9']
digit0 = char '0' <|> digit

-- 零
zero = string "0" <* notP digit0

-- 自然数
nNum = do a <- digit 
          b <- many digit0
          return (a:b)

-- 正の小数
dNum = do a <- (nNum <|> zero)
          b <- string "."
          c <- dPart
          return (a ++ b ++ c)
  where dPart = (:) <$> digit0 <*> dPart <|> (many1 digit) <* notP (char '0') -- 小数部

-- 正数
pNum = dNum <|> nNum <* notP (char '.')

-- 実数
num = zero <|> (++) <$> option "" (string "-") <*> pNum


number :: Parser Double
number = do n <- num
            return (read n)