import Control.Monad
import Control.Applicative ( (<$>), (<*>), (<*) )
import Text.Ponder

{- PEG
digit  = (1/2/3/4/5/6/7/8/9)
digit0 = (0/digit)

zero   = (0,digit0!)

num    = (-?,pNum/zero)
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
nNum = (:) <$> digit <*> (many digit0)

-- 正の小数
dNum = (nNum <|> zero) `seqP` string "." `seqP` dPart
  where dPart = (:) <$> digit0 <*> dPart <|> (many1 digit) <* notP (char '0') -- 小数部

-- 正数
pNum = dNum <|> nNum <* notP (char '.')

-- 実数
num = (option "" (string "-") `seqP` pNum) <|> zero


number :: Parser Double
number = do n <- num
            return (read n)
