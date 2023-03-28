{-# LANGUAGE TypeOperators, MultiParamTypeClasses, DeriveFoldable, FlexibleInstances, FlexibleContexts #-}
module Application.Logic.TreePropositions where
import Application.Logic.Term2
import Application.Logic.BasicFunctions
import Data.Tree
import ClassyPrelude hiding (empty , take)
import Data.List ((!!))
import Data.Foldable (foldl)
import Data.List ((!!),elemIndex,take)
import Application.Logic.Tre
import Application.Logic.Propositions3
import Control.Monad
import Data.Maybe

drawTree' :: Tree String -> String
drawTree' (Node a []) = a
drawTree' x = drawTree x
drawForest' :: Forest String -> String
drawForest' x = x >>= drawTree'
newtype Gump a = Gump {undForest :: Forest a} deriving (Eq, Foldable )
instance Functor Gump where
	fmap = \f -> Gump  . fmap (fmap f) . undForest
instance Semigroup (Gump a) where
	(<>) = \(Gump x) (Gump y) -> Gump (x ++ y)
instance Applicative Gump where	
stdTree :: x -> Tree x
stdTree l = Node l []	
instance Monad Gump where
	return = \x -> Gump [stdTree x]
instance (Show a) => Show (Gump a) where
	show = drawForest . fmap (fmap show) . undForest
type Prop= P Gump

{-- IN THE END REFACTOR THE FOLLOWING NAMES in CarpetCEntral4 --}
--The Function expects as an input a function
tableToGump :: Eq a => [(a, Maybe a)] -> Gump a
tableToGump arr' = let 
		(alpha' , beta') = twoBins (isJust . snd) arr' 
		(alpha'' , beta) = ((map fst alpha') , (map (fmap fromJust) beta' )) 
		alpha = alpha'' ++ map fst (filter (\(x , y) -> not (y `elem` (map fst arr'))) beta) in
	Gump $ tillTerminate (\f -> foldl (\f x -> actOnForest (func x) f) f beta) $ foldl addIfItsNew [] alpha where
			func :: Eq a =>  (a , a) -> Forest a -> Forest a
			func (a , p) = map (\n -> if (val n == p) then Node (val n) (addIfItsNew (subForest n) a) else n)



addIfItsNew :: Eq a => Forest a -> a -> Forest a
addIfItsNew f a = if (a `elem` (map val f)) then f else f ++ [Node a []]
ai' :: P t b -> t b
ai' = snd . pp

addCell :: Gump a -> a -> Gump a
addCell (Gump ass) c = Gump (ass ++ [Node c []])

actOnForest :: (Forest a -> Forest a) -> Forest a -> Forest a
actOnForest g = g . map (foldTree (\a bs -> Node a (g bs)))
filterDs :: Eq a => Gump a -> Gump a
filterDs = Gump . actOnForest (filterD) . undForest
val :: Tree a -> a
val (Node a _) = a
getChildren :: Tree a -> [a]
getChildren (Node a ar) = map val ar
instance Term Gump where
  empty =  Gump []
  --replace x p q = let q' = undForest q in Gump $ actOnForest (\f -> (if (x `elem` map val q') then undForest (filterDs p) else []) ++ filter (\y -> val y /= x) f) $ q' -- feels very weird.
  replace x p q = let q' = undForest q in Gump $ actOnForest (\f -> if (x `elem` map val f) then let Just i = elemIndex x (map val f) in filterD (take i f ++ undForest (filterDs p) ++ drop (i+1) f) else f) q' -- ++ filter (\y -> val y /= x) f) $ q' -- feels very weird.
{--
instance TermA Int Gump where
  --replaceA f i (Gump q) = let [ys,zs] = delN i q in Gump (ys ++  (f (take (i+1) q)) ++ zs)
  --replaceM f i (Gump q) = let [ys,zs] = delN i q in Gump (\x -> filterD $  ys ++ x ++ zs) `fmap` (f (take (i+1) q))
{--

instance TermA Int [] where --emptyA = (0,[])
  replaceA f i q = let [ys,zs] = delN i q in (ys ++  (f (take (i+1) q)) ++ zs)
  replaceM f i q = let [ys,zs] = delN i q in (\x -> filterD $  ys ++ x ++ zs) `fmap` (f (take (i+1) q))
--} 
--} 
getFirstLeafPath :: Gump a -> Path
getFirstLeafPath = snd . head' . allLeafPathsOfGump
allLeafPathsOfGump :: Gump a -> [(a , Path)]
allLeafPathsOfGump (Gump a) = let leafs = map allLeafPathsOfStdTree a in (concat $ zipWith (\las i -> map (fmap (\as -> i : as)) las) leafs (bra leafs)) 
instance Accessible Path Gump where 
	access (a:as) x = val $ go' ((!! a). undForest $ x) as -- ( !! y) . gumpToList
	
insertLeft :: a -> Prop a
insertLeft = \x ->  P (return x , empty)
insertRight :: a -> Prop a
insertRight = \x ->  P (empty , return x)  
instance TermA Path (P Gump) where
	replaceA f n p = let x = (ai' p <!!> n) in replace x (f (insertRight x)) p --
    --replaceM f n p = let x = (ai' p <!!> n) in fmap (\q -> replace x q p) (f (insertRight x))

instance Accessible Path (P Gump) where
	access y = access y .  ai'

concatGroupByFirst :: Gump (Int,a) -> Gump (Int,a)
concatGroupByFirst = Gump . actOnForest g . undForest where
	
	f :: Tree (Int,a) -> (Int , Tree (Int , a))
	f (Node (i,x) as) = (i, Node (i,x) as)
	g :: [Tree (Int , a)] -> [Tree (Int , a)]
	g = map snd . concat . groupByFirst . map f 
gumpToList :: Gump a -> [a]
gumpToList = allLeafsOfStdTree <=< undForest -- flatten <=< undForest	
listToGump :: [a] -> Gump a
listToGump = Gump . map (\x -> Node x []) 
headAI' :: Prop a -> a
headAI' = head' . gumpToList . ai'
--instance TermA [Int] (P []) where
--    replaceA f ns p = let xs = map (ai' p !!) ns in replace
{--
instance TermA Int (P []) where

    
instance Accessible Int (P []) where
    access y = access y . ai'
--}





--}

--}
