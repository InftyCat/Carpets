{-# LANGUAGE TypeOperators, MultiParamTypeClasses, DeriveFoldable,FlexibleInstances, FlexibleContexts#-}
module Application.Logic.Tre where
import Application.Logic.Term2
import Data.Tree
import Application.Logic.Propositions3
import Data.Maybe
import Control.Monad
import Data.Traversable
import Data.Text (unpack)
import ClassyPrelude
import Data.List ((!!))
import Data.Foldable (foldl)
import Application.Logic.BasicFunctions
--type Prop a = P STre a
--type STre a = Tre a a
allLeafsOfStdTree :: Tree a -> [a]
allLeafsOfStdTree = foldTree (\ a leafs -> if (null (leafs)) then [a] else concat leafs)

allLeafPathsOfStdTree :: Tree a -> [(a, Path)]
allLeafPathsOfStdTree = foldTree (\ a leafs -> if (null (leafs)) then [(a,[])] else concat $ zipWith (\las i -> map (fmap (\as -> i : as)) las) leafs (bra leafs))

repla'' :: (a -> Bool) -> (Tree a -> (Tree a)) ->  a -> Forest a -> (Tree a)
repla'' f g y s =  Node y $ map (\cs -> if f (rootLabel cs) then g cs else cs) s
replaceWith'' f g t= if f(rootLabel t) then g(t) else foldTree (repla'' f g) t
repla' :: (a -> Bool) -> (Tree a -> (Tree a)) ->  a -> Forest a -> (Tree a)
repla'  f g y s = if (f y) then g (Node y s) else repla'' f g y s
replaM' :: Monad m => (a -> Bool) -> (Tree a -> m (Tree a)) ->  a -> Forest a -> m (Tree a)
replaM' f g y s = if (f y) then g (Node y s) else Node y `fmap` (traverse (\cs -> if f (rootLabel cs) then g cs else return cs) s)
repla x = repla' ((==x))
replaceWithM' :: Monad m => (a-> Bool) -> (Tree a -> m (Tree a)) -> Tree a -> m (Tree a)
replaceWithM' f g = foldMTree (replaM' f g)
replaceWith' f g = foldTree (repla' f g)
type Path = [Int]
--std :: (Path -> (a,Int)) -> Tree a
--std f = unfoldTree (\b -> (fst $ f b, map (:b) [0..((snd $ f b) -1)])) []
--replaceWith2 :: Eq a => a -> Tree a -> Tree a -> Tree a
--replaceWith2 x p = let f b = (rootLabel b)
replaceWith :: ( Eq a) => a -> Tree a -> Tree a -> (Tree a)
replaceWith x p = replaceWith' (((==x))) (pure p)
foldMTree :: Monad m => (a -> [b] -> m b) -> Tree a -> m b
foldMTree f = foldTree (\a -> (>>= f a) . sequenceA  ) -- t let g j = traverse (foldTree j) t in traverse (fm)
{--
printT :: Show a => Tree a -> IO ()
printT = putStrLn . drawTree . fmap show
prT ::  Show a => Tree a -> String
prT = drawTree . fmap show
printF :: Show a => Forest a -> IO ()
printF = putStrLn . drawForest . (map (fmap show))
--}
--newtype Fores a b = Fores {forest :: Forest (Either a b)} deriving (Eq,Foldable)
newtype Tre a b = Tre {tre :: Tree (Maybe (Either a b))} deriving (Eq, Foldable)
instance (Show a, Show b) => Show (Tre a b) where
  show = drawTree . fmap (fromMaybe "NIX" . fmap printEither) . tre . shrinkTre
instance Functor (Tre a) where
  fmap f = Tre . fmap (fmap (fmap f)) . tre
printEither :: (Show a, Show b) => Either a b -> String
printEither (Left x) =  show x
printEither (Right x) = show x  -- ++ ">"
label :: Tre a b -> Maybe a
label = rootLabel . tre >=> leftToMaybe
shrink' :: Maybe (Either a b) -> [Tree (Maybe (Either a b))] -> Tree (Maybe (Either a b))
shrink' = \lab list -> let arr = filter (isJust . rootLabel) list in if (isNothing (lab >>= rightToMaybe) && null arr) then Node Nothing [] else Node lab $ arr
shrinkTre :: Tre a b -> Tre a b
shrinkTre = Tre . foldTree (shrink') . tre
node :: a -> [Tre a b] -> Tre a b
node a bs = Tre $ Node (Just $ Left a) $ map tre bs
sTre :: Tree (Maybe (Either a b)) -> Tre a b
sTre = shrinkTre . Tre


leaf :: b -> Tre a b
leaf = Tre . return . Just . Right
type ME a b  = Maybe (Either a b)
type PTre a b= (Path, Tre a b)
--instance (Show a , Show b) => Show (PTre a b) where
--	show = show' . fmap show
treMap :: (Tree (Maybe (Either a b)) -> Tree (Maybe (Either a b))) -> Tre a b-> Tre a b
treMap f = Tre . f . tre
getLeaf :: Tre a b -> b
getLeaf = fromJust . getLeaf'
getLeaf' :: Tre a b -> Maybe b
getLeaf' = (rootLabel . tre >=> rightToMaybe)
changeRootLabel :: (a -> a) -> Tree a -> Tree a
changeRootLabel f t = Node (f $ rootLabel t) (subForest t)
--addPos' :: [Int] -> a -> [Tree a] -> Tree ([Int],a)

--addPos :: Tree a -> Tree ([Int],a)
--addPos = foldTree addPos'
addPos :: Tree a -> Tree ([Int],a)
addPos t = addPos' [] (rootLabel t) (subForest t) where addPos' x p ls = Node (x,p) (zipWith (\i y-> addPos' (x++[i]) (rootLabel y) (subForest y)) (bra ls) ls)
go :: Tre a b -> Path -> Tre a b
go x p = Tre $ go' (tre  $ x) p
go' :: Tree a -> Path -> Tree a
go' = foldl (\tree b -> (subForest $ tree) !! b)  -- foldl (\tree b -> Tre $ (subForest $ tre tree) !! b) 
leafGAction :: (Eq a, Monad m, Eq b) => (b -> m (Tre a b)) -> PTre a b -> m (Tre a b)
leafGAction f = uncurry (replaceM (f . getLeaf))
children = subForest . tre
leafAction :: (Eq a,Eq b, Monad m ) => (b -> m b) -> PTre a b -> m (Tre a b)
leafAction f = let g = (>>= return . leaf) . f in leafGAction g
instance Eq a => Term (Tre a) where
  empty = Tre (return Nothing)
  replace x (Tre p) (Tre q) = sTre $ replaceWith (Just $ Right x) p q
trfmapsnd =  (Tre. fmap snd) :: Tree (Path,ME a b) -> Tre a b
instance Eq a => TermA [Int] (Tre a) where
  --emptyA = ([],empty)
  replaceA f is (Tre q) = trfmapsnd $ replaceWith' (\x -> fst x == is) ((addPos . tre) . f . trfmapsnd) (addPos q)
  replaceM f is (Tre q) = fmap trfmapsnd $ replaceWithM' (\x -> fst (x)== is) (fmap (addPos . tre) . f . trfmapsnd) (addPos q)
instance Eq a => Accessible [Int] (Tre a) where
  access = fmap getLeaf . flip go
allLeafs :: Tre a b -> [(Path,b)]
allLeafs = filterNothings . map (fmap sequenceA . fmap $ (>>= rightToMaybe) ) . flatten . fmap (sequenceA . sequenceA) . addPos . tre
allLeafPaths :: Tre a b -> [Path]
allLeafPaths = map fst . allLeafs
toPTree :: Tre a b -> PTre a b
toPTree t = (head' $ (allLeafPaths t ++ [[]]),t)
