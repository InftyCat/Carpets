module Application.Logic.BasicFunctions where
import IHP.Prelude hiding (null) --hiding (intersection)-- 
import IHP.ModelSupport
import Data.Maybe
import Data.Traversable
import Data.Foldable 
import Data.Text (unpack)
import Data.List.Split (chunksOf)
import Data.Functor.Compose

import Control.Monad
show' :: (Show a) => a -> String
show' = unpack . show

{--instance Show a  => Showable a where
	toString = unpack . tshow
	--}
	-- showsPrec = \n a -> toString a
tail' = fromJust . tail
head' = fromJust . head
init' = fromJust . init
last' = fromJust . last
flipPair :: (a,b) -> (b,a)
flipPair = \(x,y) -> (y,x)
repl :: (a,b) -> (a,(a,b))
repl (a,b) = (a,(a,b))
xor :: Bool -> Bool -> Bool
xor False x = x
xor True x = not x
cSP :: ((a,b) -> b) -> (a,b) -> (a,b) --changeSndPosition
cSP f = \(x,y) -> (x, f (x,y)) -- fmap f . repl --
stdMaybe :: Bool -> a -> Maybe a
stdMaybe b x = if b then Just x else Nothing
stdM :: Monad m => Bool -> (a -> m a) -> (a ->m a)

stdM b f = if b then f else return
stdM' :: Monad m => (m a -> Bool) -> (a -> m a) -> ( a -> m a)
stdM' nothing f x = if (nothing $ f x) then return x else f x
stdEndo :: Bool -> (a -> a) -> (a -> a)
stdEndo b f = if b then f else id
bra :: [a] -> [Int]
bra l = [0..length (l) - 1]
type Edge = (Int,Int)
maybePt :: (a -> Maybe a) -> a -> a
maybePt f x = fromMaybe x (f x)
continueIf :: Bool -> Maybe ()
coninueIf True = Just ()
continueIf False = Nothing
printFold :: (Functor t, Foldable t, Show a) => String -> t a-> String
printFold t = foldlMon (\a x->a ++ t ++ x) . fmap (unpack . show)
foldlMon :: (Foldable t, Monoid a) => (a -> a -> a) -> t a -> a
foldlMon f l = if (null l) then mempty else foldl1 f l
arrWithLetter :: String -> [String] -> String
arrWithLetter t = foldlMon (\z p -> z ++ t ++ p)
printStrArr :: [String] -> String
printStrArr = arrWithLetter ","
cutArr :: (Show a, Foldable t) => t a -> String
cutArr = fromJust . (init <=< tail) . unpack . show . toList
filterD :: Eq a => [a] -> [a]
filterD [] = []
filterD (x:xs) = [x] ++ filterD (xs `comp` [x])
comp :: Eq a => [a] -> [a] -> [a]
comp nodes w =  filter (not . flip elem w) nodes
tillTerminate :: Eq a => (a->a) -> (a->a)
tillTerminate f x = if (f x == x) then x else tillTerminate f (f x)
printSPair :: (String,String) -> String
printSPair (i,x) = "(" ++ i ++ "," ++ x ++ ")"
next :: Eq a=>  [a] -> a -> a
next ps p = ps !! (((fromMaybe (-1) $ elemIndex p ps) +1) `mod` (length ps))
filterNothings :: [Maybe a] -> [a]
filterNothings = (>>= maybeToList)
leftToMaybe :: Either a b -> Maybe a
leftToMaybe (Left a) = Just a
leftToMaybe (Right _) = Nothing
toEdges (y:arr) = 
	map (\[x,y] -> (x,y)) (chunksOf 2 $ (y : ((fromJust $ init arr) >>= \x -> [x,x])) ++ [fromJust $ last arr])
rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left a) = Nothing
rightToMaybe (Right a) = Just a
unsafeFromLeft :: Either a b -> a
unsafeFromLeft = fromJust . leftToMaybe
invSequenceA :: [(a,m)] -> (a,[m])
invSequenceA xs = (fst $ head' xs, map snd xs)

eqListModOrd l m = (l `intersection` m == l) && (m `intersection` l == m)
functionGraph :: (x->y) -> [x] -> [(x,y)]
functionGraph f = map (\x -> (x,f x))
intersection :: Eq a => [a] -> [a] -> [a]
intersection x y = filter (flip elem x) y
twoBins :: Eq a => (a -> Bool) -> [a] -> ([a],[a])
twoBins f = foldr (\x -> let g = if f x then id else flipPair in g . fmap (x:) . g) ([],[])
groupByFirst :: Ord a => [(a,m)] -> [[(a,m)]]
groupByFirst = groupBy (\x y -> fst x == fst y) . sortBy (\x y -> fst x `compare` fst y)
pow :: (a -> a) -> Int -> (a -> a)
pow f x = if (x <= 0) then id else f . pow f (x-1)
groupByEndo :: Eq a => [a] -> (a -> a) -> [[a]]
groupByEndo arr endo = let
	-- powertail :: b -> [b]
	powertail x = map (\i -> pow (\y -> if endo y `elem` arr then endo y else y) i x) (bra arr)
	in map filterD $ getMaxima subList (map powertail arr)
getMaxima :: Eq a => (a -> a -> Bool) -> [a] -> [a]
getMaxima le arr = arr >>= (\a -> if (any (le a) (delete a arr)) then [] else [a])
instance (Monad m, Monad t, Traversable t) => Monad (Compose m t) where
  return = pure
  (Compose x) >>= f = Compose ( x >>= fmap join . traverse (getCompose . f))
safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:xs) = Just x
safelast :: [a] -> Maybe a
safelast [] = Nothing
safelast xs = Just $ last' xs
subList :: (Eq a) => [a] -> [a] -> Bool
subList as bs = all (\a -> a `elem` bs) as
