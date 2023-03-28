{-#LANGUAGE FlexibleInstances #-}
module Application.Logic.MappingTree where
import Data.Tree
import Application.Logic.Tre
import Control.Monad.Writer hiding (foldM)
import ClassyPrelude
import Data.List ((!!))
import Data.Foldable (foldl)

import Data.Maybe
import Application.Logic.BasicFunctions

import Data.Functor.Compose
addEdge :: Eq a=> (a,a) -> Tree a ->  Tree a
addEdge (v,w) t = if (w `elem` t) then t else replaceWith' (==v) (\(Node v xs) -> Node v (return w : xs)) t

einhaengen x y = replaceWith'' (==rootLabel x) (\t -> Node (rootLabel t) (subForest t ++ subForest x)) y --killDoubles $
swapIfNec f x y = if ((rootLabel y) `elem` x) then f y x else f x y
instance Eq a=> Semigroup (Tree a) where
  (<>) = fmap killDoubles . swapIfNec (einhaengen)
edgeToTree :: (a,a) -> Maybe (Tree a)
edgeToTree (v,x) = Just $ Node v [return x]
eTT = edgeToTree
{--printMT :: Show a => Maybe (Tree a) -> IO ()
printMT = printT . fromJust
--}
Just t = mconcat  $ map (Just . edgeToTree) [(3,4),(2,5),(2,4),(5,2),(5,3)]
findRoute :: Eq a => (a,a) -> Tree a -> [a]
findRoute e = head' . filter (\ar -> (head' ar, last' ar) == e) . allRoutes
allRoutes :: Tree a -> [[a]]
allRoutes = foldTree (\a xs -> if null xs then [[a]] else (map (a:)) =<< xs)
type W = Writer (Maybe (Tree Int))


tr =  Node 2 [return 3, Node 3 [return 4]]

killDoubles :: Eq a => Tree a -> (Tree a)
killDoubles = fromJust . head' . snd . runWriter . killAll [] -- (Node v xs) = Node v (map (killAll [v] . killDoubles) xs)
killAll :: Eq a => [a] -> Tree a ->  Writer [Maybe (Tree a)] [a] --([a],Maybe (Tree a))
killAll ls (Node v ts) = if (v `elem` ls) then return ls else
                          let
                          wr = runWriter $ foldM killAll (v:ls) ts
                          children = filterNothings $ snd $ wr
                          in writer (fst $ wr,[Just $ Node v children])

{--
addOwnNode :: b -> Writer [(b,Maybe (Tree a))] [a] -> Writer [(b,Maybe (Tree a))] [a]
addOwnNode a w = writer $ fmap ((a,Nothing) :) $ runWriter w
killAll' :: Eq a => Maybe a -> [a] -> Tree a ->  Writer [(Maybe a,Maybe (Tree a))] [a] --([a],Maybe (Tree a))
killAll' w ls (Node v ts) = if (v `elem` ls) then writer (ls, sequenceA (Just v,map Just ts)) else addOwnNode (Just v) $ foldM (killAll' (Just v)) (v:ls) ts

                          let
                          wr = runWriter $ foldM killAll' (v:ls) ts
                          children = (filterNothings $ snd $ wr) :: [(a,Tree a)]
                          in writer (fst $ wr,map Just children)


helpFunc :: Ord a=> Tree a -> [Tree a]
helpFunc t= map ((\(x,y) -> Node (fromMaybe (rootLabel t) x) y) . fmap filterNothings . invSequenceA) .groupByFirst . snd . runWriter . killAll' Nothing [] $ t


killDoubles' :: Ord a => Tree a -> (Tree a)
killDoubles' = foldl1 einhaengen . helpFunc
--}
