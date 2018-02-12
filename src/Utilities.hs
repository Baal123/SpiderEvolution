module Utilities where

mapWithIndex :: Num n => (n -> a -> b) -> [a] -> [b]  
mapWithIndex = go 0
               where
                go i f []     = []
                go i f (x:xs) = f i x:go (i+1) f xs

mergeMonads :: Monad m => [m a] -> m [a]
mergeMonads [] = return []
mergeMonads (x:xs) = do x' <- x
                        xs' <- mergeMonads xs
                        return (x':xs')

iterateMonad :: Monad m => (a -> m a) -> m a -> m [a]
iterateMonad f m = let m' = m >>= f
                       in do m0 <- m'
                             ms <- iterateMonad f m'
                             return (m0:ms)
