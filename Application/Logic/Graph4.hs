module Application.Logic.Graph4 where

import Application.Logic.Term2
import Data.List.Split
import ClassyPrelude hiding (intersection,fromList)-- hiding (null)
--import Data.Foldable hiding (any , elem,concat,foldr,null,length)
--import Data.Traversable
-- import Data.Text (unpack)
import Data.List ((!!))
import Data.Array
import Data.Matrix
import Application.Logic.BasicFunctions
data Dir = Hori | Verti | Diag deriving (Eq, Show, Ord)


newtype Graph = Graph {adja :: ([[Int]])}


canVGraph :: Graph -> VGraph
canVGraph g = VGraph (((2,stdnodes g),[]), g)
newtype MatWrepper = MatWrepper {inhalt :: Maybe String} 
instance Show (MatWrepper) where
  show (MatWrepper (Nothing)) = ""
  show (MatWrepper (Just x)) = x
stdString :: Bool -> String -> String
stdString True x = id x
stdString False x = ""
printCell' :: VGraph -> (VGraph -> Int -> [Int] -> String) -> Int -> MatWrepper
printCell' g f n = MatWrepper $ stdMaybe (n `elem` (nodes g)) $ f g n (adj (gg g) n)
stdFunc :: (Int -> String) -> VGraph -> Int -> [Int] -> String
stdFunc f vg i n = printSPair $ (f i, unpack $ show n)
allNodes :: VGraph -> [Int]
allNodes =  stdnodes . gg

showGraphInfo' :: (VGraph -> Int -> [Int] -> String) -> VGraph -> String
showGraphInfo' f g = let
                        l = len g
                        h = ceiling (fromIntegral (length $ allNodes g) /  (fromIntegral l))
                    in "\n" ++ unpack (show $ fromList h l (map (printCell' g f) $ allNodes g))
showGraphInfo :: (Int -> String) -> VGraph -> String
showGraphInfo f g = showGraphInfo' (stdFunc f) g

instance Show Graph where
  show = showGraphInfo (show') . canVGraph
--newtype OrderedGraph = OrderedGrap {get :: (Graph, (Int -> Int -> Bool))}
gmap :: ([Int] -> [Int]) -> Graph -> Graph
gmap f = Graph . map f . adja
instance Eq Graph where
 (==) = \x y -> adja (shrink x) == adja (shrink y)
adj :: Graph -> Int -> [Int]
adj (Graph l) i = l !! i


stdnodes :: Graph -> [Int]
stdnodes (Graph l) = bra l
graph :: [[Int]] -> Graph
graph = shrink . Graph
addEdge :: (Int,Int) -> Graph -> Graph
addEdge (v,w) = Graph . (insertSomething v (filterD . (w:))) . adja

addEdges :: [[Int]] -> Graph -> Graph
addEdges f g = graph $ zipWith (++) (adja g) f
deleteEdge :: (Int,Int) -> Graph -> Graph
deleteEdge (i,j) (Graph a) = Graph (replaceA (return . (delete j . last') ) i a)
shrink :: Graph -> Graph
shrink (Graph l) = Graph $ map filterD l

edgeNodePair (v,w) u = [(v,u),(w,u)]
safeClosureIn :: [Edge] -> VGraph -> VGraph
safeClosureIn edges0 = tillTerminate (\vg -> let
                                                edges = getAllEdges vg
                                                fil eu =  eu `subList` edges0 && (not $ null $ (eu `intersection` edges ))
                                             in foldr addEdgeV' vg $ concat (edges >>= \e -> filter fil $ map (edgeNodePair e) $ nodes vg))
forgetTrash :: Graph -> Graph -> Graph
forgetTrash vg = graph . zipWith intersection (adja $ vg) . adja

insertSomething :: Int -> (a -> a) -> [a] -> [a]
insertSomething i f l =if (i < 0) then l else take i l ++ [f (l !! i)] ++ drop (i+1) l
inducedGraph :: Int -> Graph -> Graph
inducedGraph i (Graph l) =gmap (filter (/= i)) $ graph $ insertSomething i (\_ -> []) l

addEquivalence :: Edge -> VGraph -> VGraph -> VGraph
addEquivalence (i,j) basis=let [i',j'] = sortBy compare [i,j] in safeClosureIn (getAllEdges basis) . addEdgeV' (i',j')-- graph . (insertSomething i' (j':)) . adja.
--type EdgeInf = (Int,(Dir,[Int]))
data EdgeInf = EdgeInf { getDirection :: Dir , getEdgePath :: [Int] , trg :: Int , isHomo :: Bool } deriving (Eq, Show)
aHomomorphism :: VGraph -> Edge -> Bool
aHomomorphism vg (src , trg)  = null $ filter (== (src , (trg , False))) $ map (fmap (\e -> (e.trg , e.isHomo))) $ edgeMaps vg
{--
getDirection :: EdgeInf -> Dir
getDirection = fst . snd

getEdgePath = (snd . snd) :: EdgeInf -> [Int]
--}
newtype VGraph = VGraph {vgraph :: (((Int,[Int]),[(Int,EdgeInf)]),Graph)} deriving Eq
instance Show VGraph where
  show = showGraphInfo (unpack . tshow)
gg :: VGraph -> Graph
gg (VGraph (((x,y),p),z))= z
nodes :: VGraph -> [Int]
nodes (VGraph (((x,y),p),z))= y
dat :: VGraph -> ((Int,[Int]),[(Int,EdgeInf)])
dat = fst . vgraph
edgeMaps :: VGraph -> [(Int,EdgeInf)]
edgeMaps = snd . dat
getPureEdgeMaps :: VGraph -> [Edge]
getPureEdgeMaps  = map (fmap trg) . edgeMaps
len :: VGraph -> Int
len (VGraph (((x,y),p),z))= x
getAllEdges0 :: VGraph -> [(Int,Int)]
getAllEdges0 vg = getAllEdges vg ++ getPureEdgeMaps vg
getAllEdges :: VGraph -> [(Int,Int)]
getAllEdges (g) = nodes g >>= sequenceA . (adj (gg g) >>= flip (,))-- (\n -> map (\m -> (n,m)) (adj (gg g) n))
edgeInGraph :: Graph -> Edge -> Bool
edgeInGraph g (v,w) = w `elem` (adj g v)
edgeInVGraph :: VGraph -> Edge -> Bool
edgeInVGraph vg e = any (edgeInGraph (gg vg)) [e,flipPair e]
fstmap :: (a -> c) -> (a,b) -> (c,b)
fstmap f = flipPair . fmap f . flipPair
addNode :: Int -> VGraph -> VGraph
stdVGraph :: ((Int,[Int]),Graph) -> VGraph
stdVGraph (x,y) = VGraph ((x,[]),y)
addNode i = VGraph .  fstmap (fstmap (fmap (sort . (i:)))) . vgraph
addEdgeV :: (Int,EdgeInf) -> VGraph -> VGraph
addEdgeV e g = VGraph (fmap (e:) (dat g), gg g)
addEdgeV' :: Edge -> VGraph -> VGraph
addEdgeV' e = VGraph . fmap (addEdge e) . vgraph
