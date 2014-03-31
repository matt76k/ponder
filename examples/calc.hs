import Control.Monad
import Control.Applicative ( (<$>), (<*>), (<*) )
import Text.Ponder

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


-- Double型の数で結果を返すパーサー
number :: Parser Double
number = do n <- num
            return (read n)

blank = many $ oneOf [' ','\t']

block = do string "(" `seqP` blank
           e <- expr
           blank `seqP` string ")"
           return e

blockOrNum = number <|> block

addP = do val1 <- mulOrNum
          blank `seqP` string "+" `seqP` blank
          val2 <- expr
          return (val1 + val2)

mulP = do val1 <- blockOrNum
          blank `seqP` string "*" `seqP` blank
          val2 <- mulOrNum
          return (val1 * val2)

mulOrNum = mulP <|> blockOrNum

-- ()付きの加法と乗法を行うパーサー
expr = addP <|> mulOrNum
