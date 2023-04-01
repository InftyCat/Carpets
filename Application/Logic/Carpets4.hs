{-# LANGUAGE FlexibleInstances #-}
module Application.Logic.Carpets4 where
import Application.Logic.Graph4
import Application.Logic.BasicFunctions
import Control.Monad
import Data.Maybe
import ClassyPrelude
import Data.List ((!!))
import Data.Foldable (foldl)
import Data.List ((!!) , elemIndex)
import Data.Foldable (foldl)
type ZToZ = Bool
newtype Sta v = Sta {sta :: (Status,v)} deriving Eq
instance Show (Sta VGraph) where
  show = \(Sta (b,g)) -> showGraphInfo' (printCell b ) g
instance Functor Sta where
    fmap f = Sta . fmap f . sta

--le :: (Eq a, Monoid a) => a -> a -> Bool
--x `le` y = x <> y == x
compFromLE :: Eq a =>  (a -> a -> Bool) -> (a -> a -> Ordering)
compFromLE lef a b = if (a ==b ) then EQ else if (lef a b) then LT else GT
[nul,kerim,one,inft] = map SEQ [0,1,2,3]
newtype SEQ = SEQ {idx :: Int} deriving (Eq)
instance Semigroup SEQ where
  (<>) = \ (SEQ x) (SEQ y) -> SEQ $ min x y


instance Ord SEQ where
  compare = compFromLE (\a b -> idx a <= idx b)

instance Show SEQ where
 show (SEQ n)
               | n == 0 = "0"
               | n == 1 = "K"
               | n == 2 = "1"
               | n == 3 = "8"

newtype SEQ2 = SEQ2 {pair :: [SEQ]} deriving Eq
--dim' = 3
--kinc i = incl dim' i kerim

--[onE, infty ,nuL] = map (std dim') [one,inft,nul]
--[nuL,ko,ok,kk] = map (SEQ2 . (\(x,y) -> [x,y])) [(one,one),(nul,nul),(kerim,one),(one,kerim),(kerim,kerim)]
inc :: ZToZ -> Int -> SEQ ->  SEQ		-- The first parameter tells you if zero has to be mapped to zero
inc False 1 (SEQ 0) = SEQ $ 1
inc bool b (SEQ n) = SEQ $ 3 `min` (0 `max` (n-b))

instance Semigroup SEQ2 where
  (<>) = \ (SEQ2 x) (SEQ2 y) -> SEQ2 (zipWith (<>) x y)
instance Semigroup SEQE where
  (<>) = \ (SEQE x) (SEQE y) -> SEQE (x . y)
instance Monoid SEQE where
  mempty = SEQE id
instance Ord SEQ2 where
  compare = compFromLE (\ a b -> let (SEQ2 a') = stdSEQE a
                                     (SEQ2 b') = stdSEQE b
                                  in and $ zipWith (<=) a' b')
instance Show SEQ2 where
 show = showseq2
showseq2' s' = let (SEQ2 s) = stdSEQE s' in (foldl (\a t -> a ++ "x" ++ show t) (show $ head' s) (tail' s)) ++ ""
showseq2 s' = let
                  (SEQ2 s) = stdSEQE s'
                  abc = filter (\a -> (filterD s) == a) [[nul],[one],[inft]]
              in if (not $ null abc) then show ((head' $ head' $ abc)) ++ "" else showseq2' s'
newtype SEQE = SEQE {endo :: SEQ2 -> SEQ2}
stdSEQE (SEQ2 st) = let l = filter (flip elem st) [nul,inft] :: [SEQ]
                              in if (not $ null l) then SEQ2 $ replicate (length st) (head' l) else SEQ2 st


stdSEQE' = SEQE stdSEQE

repSEQE :: (SEQ2, SEQ2) -> SEQE
repSEQE = SEQE . repSEQE'
repSEQE' (a,b) = (\a' -> if (a' <= a) then (b <> a') else a') . stdSEQE

elemIdx :: Eq a => [a] -> a -> Int
elemIdx arr i = fromMaybe (-1) $ elemIndex i arr
proj :: DIM ((InfoC, Dir) -> SEQ2 -> SEQ)
proj d i s = proj' d (Info i) s
proj' :: DIM (Info -> SEQ2 -> SEQ)
proj' d i (SEQ2 st) = let idx = (elemIdx d i) in if (idx >= 0) then  st !! idx else one
insertData :: Eq b => b -> [b] -> (a -> a) -> [a] -> [a]
insertData i d f =  insertSomething (elemIdx d i) f
incl d i s = incl' d (Info i) s
incl' :: DIM (Info -> SEQ -> SEQ2)
incl' d i s = SEQ2 $ insertData i d (pure s) $ replicate (length d) one
modX :: DIM (Info -> SEQ2 -> SEQ2)
modX d i = SEQ2 . insertData i d (\x -> if x==kerim then one else x) . pair
f d b  x = f' d b (Info x)
f' :: DIM (ZToZ -> Info -> SEQ2 -> SEQ2)
f' d b x= incl' d x . inc b (ii' (fst $ info x)) . goTo1Dim d x . modX d x
ii (-1) = Ker
ii 1 = Im
ii' Ker = (-1)
ii' Im = 1
inv = ii . negate . ii'
goTo1Dim d (Info (x, i)) = \s -> head' . pair $ repSEQE' (SEQ2 [one, kerim], SEQ2 [kerim, one]) $ SEQ2 [proj d (x, i) s, proj d (inv x, i) s]
--fNaive :: DIM ((Dir, Int) -> SEQ2 -> SEQ2)
--fNaive d (i,j) = incl d (ii j, i). inc j . proj d (ii j, i)
type Status = ([Info],[SEQ2])


thisvgraphSize = fst
thisvgraphEdgemaps = snd


safeGetInc' :: VGraph -> [Int] -> Maybe Dir
safeGetInc' vg [x,y]
                      | isJust em = fmap (getDirection) em
                      | a==1 = Just Hori
                      | a==size = Just Verti
                      | a==size+1 = Just Diag
                      | otherwise = Nothing

                              where
                                      em = getEdgeMapInformation vg (x,y)
                                      size = len vg
                                      a = abs(x-y)
pushAlong :: VGraph -> Dir -> Bool -> Int ->  Maybe Int
pushAlong vg d bwds x = safehead $ filter (\y -> (stdEndo bwds flipPair (x,y)) `elem` getAllEdges0 vg && safeGetInc' vg [x,y] == Just d && ((y > x) `xor` bwds) ) $ allNodes vg                            
getInc' :: VGraph -> [Int] -> Dir
getInc' = fmap (fromJust) . safeGetInc'
getEdgeMapInformation :: VGraph -> Edge -> Maybe EdgeInf
getEdgeMapInformation vg e =safehead $ map snd $ filter (\m -> elem (fmap trg m) [e, flipPair e]) $ edgeMaps vg


getInfoC :: Int -> InfoC
getInfoC a = (if (a < 0) then Ker else Im) :: InfoC
getInc :: VGraph -> Edge -> Info
getInc thisvgraph (v,w) = let j = if (v <= w) then Im else Ker in Info (j, getInc' thisvgraph [v,w])
type Carp = (Status,VGraph)
type Carpet = Sta VGraph
carpet = sta
printCell :: Status -> VGraph -> Int -> [Int] -> String
printCell b vg i n= "(" ++ show i ++ ": [" ++ printStatus b i ++ "]" ++ (concat $ filterNothings $ map (fmap showDirShort .  safeGetInc' vg . (i:) . pure) n) ++ ")"
printStatus :: Status -> Int -> String
printStatus b i = x
                            where
                                    reSequenceA = (\(x:xs) -> (fst x, snd x : map snd xs)) :: ([(a,b)] -> (a,[b]))
                                    bi = b <!> i
                                    onee = std (getDim b) one
                                    x
                                      | bi == nulll b= "0"
                                      | bi == onee = "1"
                                      | otherwise = foldlMon (\a x -> a ++ "," ++ x) . map ((\(x,y) -> showDirShort x ++ " " ++ y) . fmap (printFold "&") . reSequenceA) . groupByFirst . map (flipPair . info . ((fst b) !! )) $ filter (\j -> pair (bi) !! j == kerim) $ bra (pair bi)
showDirShort :: Dir -> String
showDirShort Hori = "H"
showDirShort Verti = "V"
showDirShort Diag = "D"

testD = (map Info [(Ker, Verti), (Im, Verti), (Ker, Hori), (Im, Hori) ])

testStatus = (testD, map (foldl (<>) (std testD inft) . map (\p -> incl testD p kerim)) [[(Ker, Hori),  (Im, Hori), (Im, Verti)],[(Ker, Verti)]]) :: Status

type World = ([SEQE],VGraph)

getDim :: Status -> [Info]
getDim= fst
changeStatus :: (Status -> Status) ->Carpet ->  Carpet
changeStatus f (Sta (b,g)) = Sta (f b ,g)
cG :: (Graph -> Graph) -> Carp ->  Carp
cG f = fmap (cG' f)
cG' :: (Graph -> Graph) -> VGraph -> VGraph
cG' f g = VGraph (dat g,f $ gg g)



-----------------------
---laser happen in the status not in the graph
laserimg :: DIM (VGraph -> [SEQE] -> SEQ2 -> Edge -> SEQ2)
laserimg d vg mons basev (v,w) = endo (mons !! w) $ f' d (aHomomorphism vg (v,w)) (getInc vg (v,w)) (basev)
laser :: VGraph -> [SEQE] -> Edge -> Status -> Status
laser vg mons (v,w) base = let
                            e = endo $ mons !! w
                            w' = laserimg (getDim base) vg mons (base <!> v) (v,w) -- e $ f (getDim base) (getInc size (v,w)) (base <!> v)
                        in fmap (insertSomething w (\bw -> e(bw <> w'))) base

allPunctableVertices :: Carp -> [Int]
allPunctableVertices (base,g) = filter (\i -> base <!> i <= std (getDim base) one ) $ nodes g

laserableEdges :: World -> Carp -> [Edge]
laserableEdges (m,vg) c = let
                                  v =allPunctableVertices c
                                  innerEdge (x,y) = x `elem` v && y `elem` v
                                  b = fst c
                                  f (x,y)
                                          | edgeInVGraph (snd c) (x,y) = True
                                          | not (innerEdge (x,y)) = (x `elem` v) && ((b <!> y) == std (getDim b) inft) -- added the b <!> x part
                                          | otherwise = False
                                  in filter f $ getAllSymEdges vg

getAllSymEdges :: VGraph -> [Edge]
getAllSymEdges = ((\(x,y) -> [(x,y),(y,x)] ) =<<) . getAllEdges0
isLaserable :: World -> Carp -> Edge -> Bool
isLaserable (m,vg) c (v,w) = let edges = laserableEdges (m,vg) c in any (\e -> e `elem` edges) [(v,w),(w,v)]

safeLaser :: World -> Edge -> Carpet -> Maybe Carpet      -- changes only the status

safeLaser (m,wg) e (Sta (b,g)) = let
                                        b' = laser wg m e b
                                        mc = (stdMaybe (isLaserable (m,wg) (b,g) e) ((b',g) :: Carp)) :: Maybe Carp
                                     in fmap Sta mc --fmap (Sta .addArg0Edges (m,wg) (nodes wg) (snd e)  ) $

addArg0Edges :: World -> [Int] -> Int -> Carp -> Carp
addArg0Edges wld (ng) aim = let aZE (b,vg)= addZeroEdgesCore wld (ng `comp` [aim]) b vg vg in cSP aZE
------------------------------------------
--An equivalence means, that the maps preserves the (hidden) basepoints.
changeWeight :: Int -> SEQ -> Status -> Status
changeWeight v i b = insertSomething v (\_ -> std (getDim b) i) `fmap` b

addEq' :: (Bool,Bool) -> World -> Edge -> Carpet -> Maybe Carpet     -- changes almost only the graph!
addEq' (careless,del) w e (Sta (b,g)) = let
                              f1 = (addEquivalence e (snd w) )--f1 = (forgetTrash (gg $ snd w) . addEquivalence e) :: Graph -> Graph
                              f2 = (addNode (snd e)) :: (VGraph -> VGraph)
                              f3 = stdEndo (whenToDeleteNode del w b e) $ fmap ((deleteNode (snd e)) :: (VGraph -> VGraph)) . flipPair . fmap (changeWeight (snd e) one) . flipPair
                              --basis = (changeWeight (snd e) one b,g) :: Carp -- here was inft written before
                           in if (edgeInVGraph g e) then Just $ fmap f2 (Sta (b,g)) else stdMaybe (careless || isAddable w b e) $ Sta $ (( f1 . f2)  `fmap` (f3 (b,g))) --f1 `cG`
addEq  :: World -> Edge -> Carpet -> Maybe Carpet     -- changes almost only the graph!
addEq = addEq' (False,True)
whenToDeleteNode :: Bool-> World -> Status -> Edge -> Bool
whenToDeleteNode del w b e = del && (not $ is0Addable w b e)
deleteNode :: Int -> VGraph -> VGraph
deleteNode v vg =  VGraph (((len vg, filter (/= v) $ nodes vg), edgeMaps vg), inducedGraph v (gg vg))
isAddableNaive :: Status -> Edge -> Bool
isAddableNaive b (v,w) = all (\x -> x <= std (getDim b) one ) [b <!> v, b <!> w]
isAddable :: World -> Status -> Edge -> Bool
isAddable w s e = isAddableNaive s e || is0Addable w s e
---------------------------------

--outerLaser :: World -> Edge -> Carpet -> Maybe Carpet
-- the outer Laser tries to laser a bridge to/from a new node and add it as an equivalence,
-- after that it has to laser again, since the target value was set to infty
--outerLaser w e = overwrite w e <=< safeLaser w e

overwrite' :: (Bool,Bool) ->  World -> Edge -> Carpet -> Maybe Carpet
overwrite' (b,del) w e = safeLaser w e <=< addEq' (b,del) w e
overwrite :: World -> Edge -> Carpet -> Maybe Carpet
overwrite = overwrite' (False,True)
addEdgePathCareLess :: World -> [Int] -> Carpet -> Maybe Carpet
addEdgePathCareLess w es c = (foldl (>=>) pure $ zipWith (\b -> overwrite' (True,b) w) (replicate (length (toEdges es) -1 ) True ++ [not (edgeInVGraph (snd $ sta c) (head' es, last' es))]) (toEdges es)) c
overwriteAble :: World -> Int -> Edge  -> Carpet -> Maybe Carpet
overwriteAble w i e c = let (Sta (_,vg)) = c in stdMaybe (is0Addable w (fst $ sta c) e || i /= snd e || not (i `elem` nodes vg)) c
----------------------


inducedVGraph :: [Int] -> VGraph -> VGraph
-- killst die Knoten nicht im ersten Argument!
inducedVGraph w (VGraph ((l,nodes), g)) = VGraph ((l,nodes), foldl (\c a -> inducedGraph a c) g w) --`comp` w
--laserimg :: Int -> [SEQE] -> Edge -> SEQ2 -> SEQ2
inducedCarpet :: World -> Carpet -> [Int] -> Carpet
inducedCarpet wld (Sta (b, vg)) w = Sta (b', vg' )
                                  where
                                          argu x = if (x `elem` w) then (if (x `elem` nodes vg) then nulll b else std (getDim b) inft) else b <!> x
                                          b' = fmap (map (argu) . bra) b
                                          vg' = inducedVGraph w vg

nulll :: Status -> SEQ2
nulll b = std (getDim b) nul
addZeroEdgesCore :: World -> [Int] -> Status -> VGraph -> VGraph -> VGraph
addZeroEdgesCore wld w b vg= let

                                                        edges' = getCutEdges w (cG' (pure $ gg (snd wld)) vg )
                                                  in flip (foldr (\e-> maybePt (add0Edge wld b e))) edges'
addZeroEdges wld w b vg = addZeroEdgesCore wld w b vg (inducedVGraph w vg)
(<!>) :: Status -> Int -> SEQ2
b <!> i = (snd b) !! i
is0Addable :: World -> Status -> Edge -> Bool
is0Addable wld b (v,w) = let
                               mons = fst $ wld
                         in (b <!> w == nulll b) && nulll b == laserimg (getDim b) (snd $ wld) mons (b <!> v) (v,w)
add0Edge :: World  ->Status ->  Edge -> VGraph -> Maybe VGraph
add0Edge wld b vw vg =stdMaybe (is0Addable  wld b vw ) (addEdgeV' vw vg)
{--applyTillOkay :: [a -> Maybe a] -> a -> a
applyTillOkay [] x = x
applyTillOkay (f:fs) x = if (isJust $ f x) then fromJust (f x) else applyTillOkay fs x --}
applyTillOkay = foldr maybePt :: a -> [a -> Maybe a] -> a


getCutEdges :: [Int] -> VGraph -> [Edge] -- alle Kanten in den Schnitt
getCutEdges cut vg = let
                          g = gg vg
                          nds = nodes vg
                          f =  filter (flip elem cut) . adj g
                                         in (nds `comp` cut) >>= sequenceA . (f >>= flip (,))
------------------------------------------------------------------------------------------------------------

--type Dir = Int

partFunc :: a -> Int -> [(Int,a)] -> [a]
partFunc  st n fs = let ys x = map snd (filter (\(x',y) -> x==x') fs) ++ [st]
                    in map (head' . ys) $ [0..n-1]
buildMons :: Int -> [(Int, SEQE)] -> [SEQE]
buildMons n = partFunc stdSEQE' n . funcAction

funcAction :: (Monoid m, Ord a) => [(a,m)] -> [(a,m)]
funcAction =  funcActionSemi mempty
funcActionSemi :: (Semigroup m, Ord a) => m -> [(a,m)] -> [(a,m)]
funcActionSemi stdVal = map (fmap (foldl (<>) stdVal) . invSequenceA) . groupByFirst
type NodeData = (Int,SEQ2)
buildBase :: [SEQE] -> Int -> DIM ([NodeData] -> Status)
buildBase mons n d = let val = std d inft in sequenceA (d,zipWith (\m -> (endo m)) mons . partFunc val n . funcActionSemi val)


buildGraph n l nodes = let
                            t nod =concat (stdArr [nod + 1,nod + l +1] ((nod + 1) `mod` l /= 0)) ++ [nod + l]  :: [Int]
                            adjaz nod = filter (flip elem nodes) $ concat $ stdArr (t nod) (nod `elem` nodes) :: [Int]

                       in stdVGraph ((l,nodes),graph $ (map adjaz) [0..(n-1)])
-------
stdArr :: a -> Bool -> [a]
stdArr x b = if b then [x] else []

data InfoC = Ker | Im deriving (Eq, Show)
newtype Info = Info {info :: (InfoC, Dir)} deriving Eq
instance Show Info where
  show (Info (x,y)) = show y ++ " " ++ show x
type DIM a= [Info] -> a
std ::  DIM (SEQ ->  SEQ2)
std d= SEQ2 . replicate (length d)
addWeightedNode ::  Int -> SEQ -> Carpet -> Carpet
addWeightedNode snde val =  changeStatus (changeWeight snde val) . Sta .  fmap (addNode snde) . carpet
