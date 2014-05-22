module Text.Ponder.Memo
( memoParse
, memoize
, memoChar
, memoStr
) where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State
import Text.Ponder

memoParse :: Parser a -> String -> Memo
memoParse p s = runIdentity $ execStateT (runErrorT $ runStateT (runStateT p s) 1) []

memoize :: String -> Parser a -> Parser a
memoize n p = StateT $ \s ->
               StateT $ \pos ->
                do memo <- get
                   case (lookup (n, pos) memo) of
                     Just 0    -> throwError "not match"
                     Just mpos -> do
                       o <- runStateT (runStateT p (take (mpos-pos) s)) pos
                       return ((fst$fst o, drop (mpos-pos) s), mpos)
                     Nothing   -> do
                       case run p s pos [] of
                         Left _  -> put $ memo ++ [((n, pos), 0)]
                         Right a -> put $ memo ++ [((n, pos), snd a)]
                       o <- runStateT (runStateT p s) pos
                       return ((fst$fst o, snd$fst o), snd o)

memoChar :: String -> Parser Char -> Parser Char
memoChar n p = StateT $ \s ->
                StateT $ \pos ->
                 do memo <- get
                    case (lookup (n, pos) memo) of
                     Just 0    -> throwError "not match"
                     Just mpos -> return ((head s, tail s), mpos)
                     Nothing   -> do
                       case run p s pos [] of
                         Left _  -> put $ memo ++ [((n, pos), 0)]
                         Right a -> put $ memo ++ [((n, pos), snd a)]
                       o <- runStateT (runStateT p s) pos
                       return ((fst$fst o, snd$fst o), snd o)

memoStr :: String -> Parser String -> Parser String
memoStr n p = StateT $ \s ->
                StateT $ \pos ->
                 do memo <- get
                    case (lookup (n, pos) memo) of
                     Just 0    -> throwError "not match"
                     Just mpos -> return ((take (mpos-pos) s, drop (mpos-pos) s), mpos)
                     Nothing   -> do
                       case run p s pos [] of
                         Left _  -> put $ memo ++ [((n, pos), 0)]
                         Right a -> put $ memo ++ [((n, pos), snd a)]
                       o <- runStateT (runStateT p s) pos
                       return ((fst$fst o, snd$fst o), snd o)
