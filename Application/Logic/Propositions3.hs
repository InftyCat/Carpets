{-# LANGUAGE TypeOperators, MultiParamTypeClasses, DeriveFoldable, FlexibleInstances, FlexibleContexts #-}
module Application.Logic.Propositions3 where
import Application.Logic.Term2
import Application.Logic.BasicFunctions
import IHP.Prelude



{--
instance (Functor t, Foldable t, Show a) => Show (P t a) where
  show (P (x,y)) = printFold "," x ++ " -> " ++ printFold "," y
--}

-- type Prop= P []
newtype P t a = P {pp :: (t a, t a)} deriving (Eq)
instance (Functor t) => Functor (P t) where
	fmap = \f -> P . (\(x , y) -> (fmap f x , fmap f y)) . pp
-- Note that P t cant be functorial in snd argument as the types in first an second have to be equal.
fmapP :: (t a -> t a) -> P t a -> P t a 
fmapP f (P (a,b)) = P (a , f b)
pmap :: (t a -> t a) -> P t a -> P t a
pmap f = P . fmap f . pp


isReplacable :: (Term t) => Comp b -> b ->P t b ->P t b -> Bool
isReplacable f x (P (_,p)) (P (q,_)) = any (f $ x) p || any (f $ x) q

safeReplace :: (Term t,Eq b) => Comp b -> b -> P t b -> P t b -> Maybe (P t b)
safeReplace f x p q = stdMaybe (isReplacable f x p q) (P $ fmap (replace x (fst $ pp p)) (pp q))
{--
safeReplace f x p q : geg: q enthält das Aim x, p enthält ein Aim >= x. 
-q zu beweisen
beweisen wir die vorraussetzungen von p und das was von q noch übrig ist.

--}
safeReplaceA :: (Term t, Accessible a (P t), Eq b) => Comp b -> a -> P t b -> P t b -> Maybe (P t b)
safeReplaceA f y p q = let x = access y q in stdMaybe (isReplacable f x p q) (P $ fmap (replace x (fst $ pp p)) (pp q))
isEliminatable :: (Term t) => Comp b -> b -> P t b -> Bool
isEliminatable f x = isReplacable f x (P (empty,empty))
safeElim :: (Eq b,Term t) => Comp b -> b -> P t b -> Maybe (P t b)
safeElim f x = safeReplace f x (P (empty,empty))
elimAll :: (Term t, Eq b) => Comp b -> P t b -> P t b
elimAll f t = filterOut (\x -> isEliminatable f x t) t

instance Foldable t => Foldable (P t) where
  foldl f x = foldl f x . snd . pp
  foldr f x = foldr f x . snd . pp
instance Term t => Term (P t) where
  empty = P (empty, empty)
  replace x (P p) (P q) =P $ fmap (replace x (fst p)) q
flipP' = P . flipPair . pp  
{-
ai' :: P [] b -> [b]
ai' = snd . pp
--instance TermA [Int] (P []) where
--    replaceA f ns p = let xs = map (ai' p !!) ns in replace
instance TermA Int (P []) where
    --emptyA = (0,empty)
    replaceA f n p = let x = (ai' p !! n) in replace x (f (P ([],[x]))) p
    replaceM f n p = let x = (ai' p !! n) in fmap (\q -> replace x q p) (f (P ([],[x])))
instance Accessible Int (P []) where
    access y = access y . ai'

-}

