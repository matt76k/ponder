module Text.Ponder.Prim
( ParserT(..)
, Pos
, Memo
, seqP
, (<|>)
, many
, many1
, option
, andP
, notP
) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Error
import qualified Control.Applicative as Applicative ( Applicative(..), Alternative(..) )

type Pos = Int

type Memo = [((String,Pos),Pos)]

type ParserT s e m a = StateT s (StateT Pos (ErrorT String (StateT Memo m))) a


infixr 1 <|>

seqP :: (Functor m, Monad m) => ParserT s e m [a] -> ParserT s e m [a] -> ParserT s e m [a]
seqP p1 p2 = do p <- p1
                q <- p2
                return (p ++ q)

(<|>) :: (Functor m, Monad m) => (ParserT s e m a) -> (ParserT s e m a) -> (ParserT s e m a)
p1 <|> p2 = mplus p1 p2

many :: (Functor m, Monad m) => ParserT s e m a -> ParserT s e m [a]
many p = Applicative.many p

many1 :: (Functor m, Monad m) => ParserT s e m a -> ParserT s e m [a]
many1 p = Applicative.some p

option :: (Functor m, Monad m) => a -> ParserT s e m a -> ParserT s e m a
option a p = p <|> return a

andP :: Monad m => StateT s m a -> StateT s m ()
andP p = StateT $ \s -> (runStateT p s) >> return ((), s)

notP :: Monad m => ParserT s e m a -> ParserT s e m ()
notP p = StateT $ \s -> 
          StateT $ \pos -> 
           ErrorT $ do e <- runErrorT (runStateT (runStateT p s) pos)
                       case e of Left _  -> return $ Right (((), s), pos)
                                 Right _ -> return mzero
