{-# LANGUAGE TypeOperators, MultiParamTypeClasses, DeriveFoldable#-}
module Application.Logic.Term2 where
import Data.List (splitAt)
import Application.Logic.BasicFunctions
import IHP.Prelude hiding (splitAt)

import Control.Monad

class Foldable t => Term t where
    empty :: t b
    replace :: Eq b => b -> t b -> t b -> t b
class Term t => (TermA a) t where --
    --emptyA :: (a,t b)
    {--
    The Idea of replaceA is, that you replace the i.th spot by the action of f applied to the the part [0..i] of the array
    --}
    replaceA :: Eq b => (t b -> t b) -> a ->  t b -> t b
    replaceM :: (Monad m, Eq b) => (t b -> m (t b)) -> a ->  t b -> m (t b)
class Accessible a t where --(TermA a t) => 
    access :: a -> t b -> b
(<!!>) :: (Accessible a t) => t b -> a -> b
(<!!>) = flip access
replaceStd :: (Eq b,TermA a t) => a -> t b -> t b -> t b
replaceStd i p q = replaceA (pure p) i q
type Comp b =   (b -> b -> Bool)
elim :: (Eq b, Term t) => b -> t b -> t b
elim x p = replace x empty p
elimA :: (Eq b, TermA a t) => a -> t b -> t b
elimA = replaceA (pure empty)
filterOut :: (Term t, Eq b) => (b -> Bool) -> t b -> t b
filterOut f t = foldr (\b -> if (f b) then (elim b .) else id) id t t

delN :: Int -> [a] ->  [[a]]
delN n xs = let (ys,zs) = splitAt n xs in [ys , (tail' zs)]


instance Term [] where
  empty = []
  replace x p q = filterD $ p ++ filter (/=x) q

instance TermA Int [] where --emptyA = (0,[])
  replaceA f i q = let [ys,zs] = delN i q in (ys ++  (f (take (i+1) q)) ++ zs)
  replaceM f i q = let [ys,zs] = delN i q in (\x -> filterD $  ys ++ x ++ zs) `fmap` (f (take (i+1) q))
instance Accessible Int [] where
  access i p = p !! i
