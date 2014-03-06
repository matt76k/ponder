module Text.Ponder.Prim
( ParserT(..)
, andP
, notP
) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error

type ParserT s e m a = StateT s (ErrorT e m) a

andP :: Monad m => StateT s m a -> StateT s m ()
andP p = StateT $ \s -> (runStateT p s) >> return ((), s)

notP :: (Monad m, Error e) => ParserT s e m a -> ParserT s e m ()
notP p = StateT $ \s -> ErrorT $ do a <- runErrorT (runStateT p s)
                                    case a of
                                      Left _  -> return $ Right ((), s)
                                      Right _ -> return mzero
