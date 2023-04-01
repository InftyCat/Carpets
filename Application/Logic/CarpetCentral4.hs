{-# LANGUAGE TypeOperators, RankNTypes #-}
module Application.Logic.CarpetCentral4 where
import Application.Logic.Propositions3
import Application.Logic.TreePropositions
import Application.Logic.Carpets4
import Application.Logic.CarpetTrees3
import Application.Logic.Graph4
import Application.Logic.BasicFunctions
import IHP.RouterPrelude hiding (foldM , empty , comparing , I, choice)
import Data.Traversable
import Control.Applicative hiding (empty)
import Data.List ((!!))
import Data.Foldable (foldl)
import Control.Monad
import Data.Functor
import Application.Logic.Term2
import Application.Logic.MappingTree
import Control.Monad.Writer
import Data.Functor.Compose
import Application.Logic.Tre
import Data.Maybe
import Data.Tree
import ClassyPrelude hiding (foldM , empty, comparing)
import Data.List ((!!))
import Data.Foldable (foldl)

type Lift= (DIM SEQ2, DIM SEQE)
--type LInt = Either Int L
--data ChallengeStatus = DONE | TODO | DATUM [Int] deriving (Eq)
{--
instance Show ChallengeStatus where
	show DONE = "DONE"
	show TODO = "TODO"
	show (DATUM x) = show x
type FlagCell = (ChallengeStatus, Cell)
--}
type PropCell = Prop Cell

-- type Test a= (a,a)
{--data Name = Epi | Mono
fromName :: Name -> (Dir -> Lift)
fromName Epi = epi
fromName Mono = mono --}

data Jo a b = WellDefined Dir Int | Defin (b,[Int]) | I a a | Epi b | Mono b | Exact b | Map Int (Jo a b) deriving Eq
instance (Show a, Show b) => Show (Jo a b) where
  show (I x y) = show x ++ "->" ++ show y ++ ","
  show (Epi x) = show x ++ " Epi,"
  show (Mono x) = show x ++ " Mono,"
  show (Exact x) = show x ++ " Exact,"
  --show (Empty x) = show x
  show (Map i f) = "~>" ++ show i ++ " " ++ show f ++ ","
  show (WellDefined b y) = " welldefined "
  show (Defin (y,x)) = show y ++ " " ++ arrWithLetter "-" (map show x)
  

showAim (Map i (Defin (y,x))) = "~>" ++ show i ++ " " ++ show y ++ ","
showAim (Defin (y,x)) = "Construct it"
showAim (j) = show j

isMap :: (Show a, Show b)=> Jo a b -> Bool
isMap (Map i x)  = True
isMap _ = False -- = '~' `elem` (show y)
isConstMap (Defin (_,_)) = True --(Map i (Defin (x,_))) = True
isConstMap _ = False
{--isWDMap :: (Show a, Show b)=> Jo a b -> Bool -- VERY !!!!
isWDMap (Map i (WellDefined b x)) = True
isWDMap _ = False--}
fromWellDefined :: Cell -> Maybe Edge
fromWellDefined ( x , WellDefined d y) = Just (x , y)
fromWellDefined _ = Nothing

isWellDefined (WellDefined b x) = True
isWellDefined _ = False
isDefin x = False
isExact :: Jo a b -> Bool
isExact (Exact b) = True
isExact x = False
isAssumptionable :: L -> Bool
isAssumptionable  (Epi x) = True
isAssumptionable (Mono x) = True
isAssumptionable (Exact x) = True
isAssumptionable (I x y) = True
isAssumptionable (Map i (Defin x)) = True
isAssumptionable (Defin x) = True
isAssumptionable (WellDefined b y) = True



project :: L -> Dir
project  (Epi x) = x
project (Mono x) = x
project (Exact x) = x
project (I x y) = snd $ info x
project (Map i f) = project f
--project (Empty x) = x
project (WellDefined x c) =x
project (Defin (x,_)) =x
type L = Jo Info Dir
tfDir :: Bool -> Dir -> Dir
tfDir False x = x
tfDir True x
            | x==Hori = Verti
            | x==Verti = Hori
            | x==Diag = Diag
transformInfo b (Info (x,y)) = Info (x,tfDir b y)
transformL :: Bool -> L -> L
transformL b (I x y) = I (transformInfo b x) (transformInfo b y)
transformL b (Epi x) = Epi (tfDir b x)
transformL b (Mono x) = Mono (tfDir b x)
transformL b (Exact x) = Exact (tfDir b x)
transformL b (Map i f) = Map i (transformL b f)
--transformL b (Empty x) = Empty (tfDir b x)
transformL b (WellDefined x c) = WellDefined (tfDir b x) c
transformL b (Defin (x,y)) = Defin (tfDir b x, y)
--(-->) :: a -> a -> Jo a Dir
x --> y = I (Info x) (Info y)
toLift :: L -> Lift
toLift (Defin (x,_)) = (st one, pure stdSEQE')
toLift (I x y) = (x ---> y)
toLift (Epi y) = epi y
toLift (Mono y) = mono y
toLift (Exact y)=concatLifts $  map toLift $ exact y
toLift (Map i f) = toLift f
toLift (WellDefined d y) = (st nul) ~> (st nul)

(~>) :: DIM SEQ2 -> DIM SEQ2 -> Lift
start ~> end = (start,\d -> repSEQE (start d, end d))
mono :: Dir -> Lift
mono i =(Ker § i) ~> (st nul)
--showLift :: DIM (Lift -> String)

epi :: Dir -> Lift
epi i = (st one) ~> (Im § i)

(--->) :: Info -> Info -> Lift
i ---> j = kerIm i ~> ((kerIm i) <> (kerIm j))
concatLifts :: [Lift] -> Lift
concatLifts ((x,y):xs) = (x,mconcat (y : (map snd xs)))
exact :: Dir -> [L]
exact i = [(Ker, i) --> (Im, i), (Im, i) --> (Ker, i)]--[(Ker § i) ~> (Im § i), (Im § i) ~> (Ker § i)]
(§) :: InfoC -> Dir -> DIM SEQ2
x § i = kerIm $ Info (x,i)
kerIm :: Info -> DIM SEQ2
kerIm xi d = incl' d xi kerim


st= flip std
--numNodes leng = leng * leng
oriG q = graph $ replicate (numNodes q) [] -- [[1],[],[],[],[],[]] --addEdge (4,5) $ addEdge (2,5) $
type EdgeMap = Cell
type WorldInfo = (([Info],(Int,Int)), ([EdgeMap],[Int]))
type Quest = (WorldInfo,[Cell])
isConcatenated :: L -> Bool
isConcatenated (Defin (x,_)) = False
isConcatenated (I x y) = False
isConcatenated (Epi y) = False
isConcatenated (Mono y) = False
isConcatenated (Exact y)= True
isConcatenated (Map i (Defin (_,_))) = True
isConcatenated (WellDefined b y) = False
----- showCell showCellTree ------------
extendL :: L -> [L]
extendL (Exact x)=  [ (Ker , x) --> (Im , x), (Im , x) --> (Ker , x)] --std (Exact x) -- 
extendL (Map i (Defin (x,ar))) = [ Defin (x,ar) , WellDefined x (last' ar)]
extendL x = []
parent :: L -> [L]
parent (I (Info (x,d)) (Info (y,d'))) = [if ((d == d') && ([Ker , Im] `subList` [x,y] )) then (Exact d) else (I (Info (x,d)) (Info (y,d')))]
parent (Defin (x,ar)) = [Map (last' ar) (Defin (x,ar))]
parent (WellDefined c x) = []
parent x = [x]
-- parent (WellDefined x) = Map i (Defin (x , ar))
--extensionTree :: Forest L
--extensionTree = []

extendCell :: Bool -> Cell -> Tree Cell
extendCell b = traverse (\x -> if isConcatenated x && b then Node x (map stdTree  (extendL x)) else stdTree x) 
killBranch :: Bool -> Tree Cell -> Tree Cell
killBranch b = stdEndo b (stdTree . val)
	
	--helper (
--listToExtendedGump :: [Cell] -> Gump Cell
--listToExtendedGump = Gump . map (extendCell True)			 

-- reformMQuest :: MQuest -> MQuest
-- reformMQuest = qmap'' (fmapP (reformGump))
shrinkGump :: Gump Cell -> Gump Cell
shrinkGump gc = foldr (rep) gc allowedShrinks  where
	allLeafs :: [Cell]
	allLeafs = gumpToList gc
	allConjugats :: Cell -> [Cell]
	allConjugats = traverse (extendL <=< parent)
	isShrinkable :: Cell -> Bool
	isShrinkable l =allConjugats l `subList` allLeafs && parent (snd l) /= [snd l]
	allowedShrinks :: [Cell]
	allowedShrinks = filter (isShrinkable) allLeafs	
	rep :: Cell -> Gump Cell -> Gump Cell
	rep c g = let 	
		v = traverse parent $ c 
		r = if (not $ v `subList` (flatten <=< undForest $ g)) then (Gump $ map stdTree $ v) else empty		
		in  (replace c r) g 
extendAll :: Gump Cell -> Gump Cell 
extendAll = Gump . map (>>= extendCell True)	 . undForest
killAllButOneBranch :: Gump Cell -> Gump Cell
killAllButOneBranch = let arr = (map (/= 0) [0..] ) :: [Bool] in Gump . (zipWith (\b a -> killBranch b a) arr). undForest 
{--
showCell :: (Int,String) -> String
showCell (x,y) = show x ++ ":" ++ y
--}
funcActionReplacement :: Gump (Int, String) -> Forest String
funcActionReplacement (Gump for) = map (\i -> Node (show i) (map (fmap (snd )) (filter (\x -> fst (val x) == i) for))) allNums where
	allNums :: [Int]
	allNums = filterD (map (fst. val) for)

shrinkToExactSequences :: VGraph -> Gump Cell -> ([Tree Cell] , [(Dir , [[Int]])])
-- INPUT all 
-- type Sequence = [Int]
shrinkToExactSequences vg (Gump f) = (fst allExacts , allSequences) where -- Gump f where
	-- I SPOTTET A MISTAKE REGARDING EXACT SEQUENCES WHICH OCCUR TWICE
	allExacts :: ([Tree Cell] , [(Int, Dir)])
	allExacts = fmap (map (fmap project . val)) $ twoBins (\i -> isExact (snd $ val i) && null (getChildren i) ) $ filterD $ f
	allPosOfDir :: [(Dir,[Int])]
	allPosOfDir = funcAction $ map (fmap return) (map flipPair $ snd allExacts)
	seqFunc :: (Dir , [Int]) -> Int -> Int
	seqFunc (d , ar) hihi = head' $ (filter (\z -> Just (z) == pushAlong vg d False (hihi)) ar ++ [hihi])
		-- >>= (\a -> a ++ next a))
	addStartEnd :: Dir -> [Int] -> [Int]
	addStartEnd d ar = let func b = maybeToList . pushAlong vg d b in func True (head' ar) ++ ar ++ func False (last' ar)
	allSequences :: [(Dir , [[Int]])]
	allSequences =  map (\darr -> (fst darr , map (addStartEnd (fst darr)) $ groupByEndo (snd darr) (seqFunc darr))) allPosOfDir

shrinkExactZeros :: VGraph -> ([Tree Cell] , [(Dir , [Int])]) -> ([Tree Cell] , [(Dir, String)]) -- sequenceA =<< right
shrinkExactZeros vg (left , right) = (left' , right') where
	arrow = " -> "
	zero = "O"
	startEnd :: Bool -> ((Dir -> L, [Int] -> Int) , String -> String )
	startEnd True  = ((Mono  , head' ) , \x -> zero ++ arrow ++  x)-- bwds --> sth
	startEnd False = ((Epi , last') , \x -> x ++ arrow ++ zero )
	
	fun :: (Bool , (Dir , [Int])) -> Cell
	fun (b , (d, ar)) = let ((me , fu) , _) = (startEnd b) in (fu ar , me d)-- (pushAlong vg d b . fu $ ar) <&> (\i -> (i , me d))		
	mat :: [(Bool , (Dir, [Int]))]
	mat = ([ (b , dint) | b <- [True , False] , dint <- right ])	
	allGoodCells :: [Cell]
	allGoodCells = map fun mat
	usefull :: Cell -> Bool
	usefull x = x `elem` allGoodCells
	--test :: [Cell]
	
	(left' , rems) = twoBins (usefull . val) left
	isAddable :: (Bool , (Dir , [Int])) -> Bool
	isAddable bhy =  fun bhy `elem` ((map val rems))
	--left' = []
	funci :: (Dir , [Int])  -> (Dir , String)
	funci (d,ar) = (d ,  (
		let act = foldl (.) id . map (\b -> (++ "\n") . stdEndo (isAddable (b , (d,ar))) (snd (startEnd b))) $ [True , False] 
		in act ((arrWithLetter arrow (map show ar))))) 
	right' :: [(Dir , String)]
	right' = map funci right --
	
showCells :: VGraph -> Bool -> Gump Cell -> String
showCells vg isAim =  	let 
						f = 
							foldlMon (\a b -> a ++ "\n" ++ b) . 
							map (\(Node a xs) -> (a ++ " : " ++ (drawForest' xs)  )) .  
							funcActionReplacement . 
							concatGroupByFirst. 
							fmap (fmap (if isAim then showAim else show )) . Gump
						showDirections = False
					in (\(x,y) -> f x ++ "\n" ++ ((\(d,z) -> stdEndo (showDirections) (\x -> show d ++ ":\n" ++ x ++ "\n \n") z) =<< y)) . fmap funcAction . shrinkExactZeros vg . fmap (sequenceA =<<) . shrinkToExactSequences vg . --fmap (map (fmap show))
					killAllButOneBranch 
					
--map (showCell) . funcAction . map ...

--foldl (\x y -> x ++ y ++ "\n") "" $ helperShowCells isAim . gumpToList where ---- TODOOOO
	

addExacts :: [(Int,Dir)] -> [(Int,L)]
addExacts = map (fmap Exact)
addQuests :: [Quest] -> MQuest
addQuests arr = let
                   w = fst $ head' arr
                   rest = map snd arr
                   asses = filterD $ rest >>= init'
                   aims = map last' rest
              in sMQuest (w,P (shrinkGump $ listToGump asses, listToGump aims))

numNodes = snd . snd . fst . fst :: Quest -> Int

addMEx :: (Dir, [Int]) -> [(Int,L)]
addMEx arr = addExacts (map (\(x,y) -> (y,x)) $ sequenceA arr)
getLenFromWorldInfo :: WorldInfo -> Int
getLenFromWorldInfo = fst . snd . fst
getLen :: Quest -> Int
getLen = getLenFromWorldInfo . fst
getD :: Quest -> [Info]
getD = fst . fst . fst
addMapsToWorldInfo :: Quest -> WorldInfo -- (WorldInfo,[(Int,L)])

addMapsToWorldInfo ((x,y), cells) = let
                                  --cs = twoBins (isMap . snd) (init' cells)
                                  maps = getAllAssMapsOrDefin ((x,y),cells)
                                  in (x,(maps, snd y)) :: WorldInfo --filterD $ fst y ++ 
getAllAssMapsOrDefin :: Quest -> [EdgeMap]
getAllAssMapsOrDefin = filter ( or . sequence [isMap , isConstMap] . snd) . init' . snd -- TODDO
getAllDefinMaps :: Quest -> [EdgeMap]
getAllDefinMaps = filter ( or . sequence [isConstMap] . snd) . init' . snd
getAllAssMaps :: Quest -> [EdgeMap]
getAllAssMaps = filter (isMap . snd) . init' . snd

aimFromCell :: Cell -> Int
aimFromCell (x , Map i _) = i
aimFromCell (x , Defin (_ , arr)) = last' arr -- PROBLEM WITH welldefined
aimFromCell (x , WellDefined c y) = y
aimFromCell (y , _) =  y

getSnd' :: Quest -> (([(Int,SEQE)],SEQ2,(Int,Int)),SEQE) 
getSnd' q' = let
                q = (addMapsToWorldInfo q', snd q')
                pureLifts = (snd q) `comp` (getAllAssMaps q)
               {-- getTargetFromHypotheticallyWD :: (Int , L) -> Int
                getTargetFromHypotheticallyWD (x , WellDefined d) = fst $ head' $ 
					snd $ twoBins snd $ map (\(x' , Defin ( d' , arr)) -> (last' arr , x == x' && d == d') ) (getAllDefinMaps q) 
					--}
                aimcell = last' $ snd q
                ai = aimFromCell aimcell -- if isWellDefined (snd aimcell) then getTargetFromHypotheticallyWD aimcell  else aimFromCell aimcell 
                ((h,(g,qu)):fs') = map (fmap toLift) $ reverse $ pureLifts
                fs = fmap (fmap snd) fs'
                d = getD q
            in ((map (fmap ($ d)) fs, g d, (h,ai)), qu d)
            --mapConvert :: [Cell] -> [(Int , EdgeInf)]
getSnd :: Quest -> ([(Int,SEQE)],SEQ2,(Int,Int))
getSnd = fst . getSnd'
mons q= buildMons (numNodes q) y where (y,_,_) = getSnd q---- [(2,epi dim hori),(3,mono dim hori)] --
startdata q = let (_,x,_) = getSnd q in x
startPos q = let (_,_,x) = getSnd q in fst x
endPos q = let (_,_,x) = getSnd q in snd x
enddata q = endo (snd $ getSnd' q) (startdata q)
endnodes q = [(endPos q, enddata q)]
startnodes q= [(startPos q,startdata q)] --[(2, ink verti)] --
whichNodes b = if b then startnodes else endnodes
--initBase :: Quest ->
initBase = liftM4 buildBase mons numNodes getD

base' b = initBase (whichNodes b) --(mons q) --(numNodes q) (getD q) (whichNodes b q) --
base = base' True
{--startWD :: W MQuest ~-~> PTreeVC
startWD = bAction $ bAction . Just . fmap initStart -- bAction $ bAction . Just . fmap initStart
--}

initStart :: MQuest -> PTreeVC
initStart q = ([],leaf $ getCarpet q)
safeCommand :: (PTreeVC -> MQuest ~-~> PTreeVC) -> PTreeVC -> MQuest ~-~> PTreeVC 
safeCommand f pt = bAction \q -> let erg = (f pt) `action` q in if isJust (action erg) then erg else return pt -- stdM' (isJust . action)
start :: MQuest ~-~> PTreeVC
start = startNaive >>= safeCommand choi where
	choi :: PTreeVC -> MQuest ~-~> PTreeVC 
	choi pt = bAction $ \q -> bAction (h pt q)
	-- h :: PTreeVC -> MQuest -> Maybe PTreeVC
	h pt q = do 
		edge <- getWDEdge q 
		action (action (choice edge pt) q)
getWDEdge :: MQuest -> Maybe Edge
getWDEdge q = let c = getAim $ mquestToQuest q in fromWellDefined c		 -- do
			--continueIf $ isWellDefined $ snd $ c 
			
startNaive :: MQuest ~-~> PTreeVC
startNaive = bAction $ toW . Just . initStart
getCarpet :: MQuest -> Carpet
getCarpet = getCarpetW True
getCarpetW :: Bool ->MQuest -> Carpet
getCarpetW b cmq = intersectBases cmq (whichNodes b (mquestToQuest cmq))

nodeDataFromCarpet :: Carpet -> [NodeData]
nodeDataFromCarpet (Sta ((b,state),_)) = map (\i -> (i,state !! i)) $ bra state
intersectBases :: MQuest -> [NodeData] -> Carpet
intersectBases (q) nd = let
                                          car = emptyCarpet q -- earlier here was carpet
                                          (Sta (s1,v1)) = car
                                          statu =initBase (pure $ nodeDataFromCarpet car ++ nd) (mquestToQuest q)
                                          vg = foldr (addNode) v1 (map fst nd)
                                      in Sta (statu,vg )
emptyCarpet :: MQuest -> Carpet
emptyCarpet mq = let 
	q = mquestToQuest mq
	l = getLen q 
	vg = deleteAimEdges mq (addEdgeMapsFromQ (stdVGraph ((l,[]), oriG q)) q)
	in Sta (initBase (pure []) q, vg ) :: Carpet
--newtype MQuest = MQuest {MQuest :: (Carpet, MQuest)} deriving (Eq)
{--
instance Show MQuest where
  show (MQuest q) = show $ snd $ q
--}  
mquestToMQuest :: MQuest -> MQuest
mquestToMQuest =  reformAims-- (\q -> MQuest(getCarpet $ MQuest (emptyCarpet q, q), q)) . reformAims


getAddEdges :: WorldInfo -> [EdgeMap]
getAddEdges = fst . snd
vg :: Quest -> VGraph
vg = addEdgeMapsFromQ =<< liftM3 buildGraph numNodes getLen (snd . snd . fst)
vg' :: MQuest -> VGraph 
vg' = flip deleteAimEdges =<<  vg . mquestToQuest 
mapConvert :: [Cell] -> [(Int , EdgeInf)]
mapConvert = (>>= traverse f) where
	h :: (Int , (Dir , [Int]), Bool) -> EdgeInf
	h (i , (d , xs) , b) = EdgeInf { trg = i , getDirection = d , getEdgePath = xs , isHomo = b}
	f (Map j (Defin (d,xs))) = [h (j,(d,xs) , True)]
	f (Defin ( d , xs)) = [h (last' xs , (d , xs) , False)]
	f x = []

edgeMapToEdge :: EdgeMap -> Edge
edgeMapToEdge (i, Defin (_ , ar)) = (i,last' ar)
addEdgeMapsFromQ :: VGraph -> Quest ->  VGraph -- TODDO
addEdgeMapsFromQ g q = foldl (\gr ed -> addEdgeV ed gr) g $ (mapConvert) $ (fst . snd . fst $ q) where
	
	
deleteAimEdges :: MQuest -> VGraph ->  VGraph
deleteAimEdges q = flip (foldr func) (gumpToList . ai' . snd . mquest $ q) where 
	func :: Cell -> VGraph -> VGraph
	func aim = stdEndo (isConstMap $ snd $ aim) (cG' (deleteEdge (edgeMapToEdge aim)))


----------


--------------


----------------------
type (~~>) w = ((->) w) :*: Maybe
type (~-~>) w= ((->) w) :*: MW
--newtype w ~~> a= bAction {action :: w -> Maybe a}
action :: (f :*: g) a -> f (g a)
action = getCompose
--bAction :: (w -> Maybe a) -> (w ~~> a)
bAction :: f (g a) -> (f :*: g) a
bAction = Compose


sMQuest :: (WorldInfo, PropCell) -> MQuest
sMQuest = MQuest . fmap (\(P (a,b)) -> P (concatGroupByFirst a, concatGroupByFirst b))
--type AQ = (~~>) Quest

qmap :: (PropCell -> PropCell) -> MQuest -> MQuest
qmap f (MQuest (a,b)) = sMQuest (a,f b)


cofmap :: Functor f => (q -> w) -> (((->) w) :*: f) a -> (((->) q) :*: f) a
cofmap f (g) = bAction (action g . f)
mquestToQuest' :: Path -> MQuest -> Quest
mquestToQuest' i (MQuest (w,P (p,q))) =  flipPair $ fmap (addMapsToWorldInfo . flipPair) $ repl (gumpToList p ++ [access i q ],w)
mquestToQuest :: MQuest -> Quest
mquestToQuest mq = mquestToQuest' (getFirstLeafPath . ai'. snd . mquest $ mq) mq
mquestToQuestSafe :: MQuest -> Maybe Quest
mquestToQuestSafe mq = if (null $ undForest $ snd $ pp $ snd $ mquest mq) then Nothing else Just $ mquestToQuest mq
world =(\q -> (mons q ,vg q)) . mquestToQuest :: MQuest -> World
qToIw = cofmap (\q -> (endPos $ mquestToQuest q, world q))
apCore :: Monad m => (a -> Edge -> PTreeVC -> m PTreeVC) -> [Int] -> PTreeVC -> a -> m PTreeVC
apCore f e p w = apL (f w) e p
out:: [Int] -> PTreeVC -> MQuest ~-~> PTreeVC
out e p =(qToIw $ bAction $  apCore (outerLaserAction) e p)
arg::  [Int] -> PTreeVC -> MQuest ~-~> PTreeVC
arg e p = (cofmap world $ bAction $ apCore safeLaserAction e p)
edg ::  [Int] -> PTreeVC -> MQuest ~-~> PTreeVC
edg e p =(qToIw $ bAction $ apCore ovAction e p)
uni :: [[Int]] -> PTreeVC ->  MQuest ~-~> PTreeVC
uni e p = (qToIw $ bAction $ \w -> apLM (universal w) e p)
spl :: Edge -> [Int] -> PTreeVC ->  MQuest ~-~> PTreeVC
spl e a p= (cofmap world $ bAction (\w -> splitAction w e a p))
choice :: Edge -> PTreeVC -> MQuest ~-~> PTreeVC
choice e p = (cofmap world $ bAction (\w -> choiceAction w e p))
tOT :: PTreeVC -> MQuest ~-~> PTreeVC
tOT pt = bAction (pure $ bAction $ fmap return $ tOT' pt)



-------------


comparing :: Quest -> Comp Carpet
comparing q c1 c2 = let f = (<!> (endPos q)) . fst . sta in (f c1) <= (f c2)

reduction ::PTreeVC -> MQuest -> PTreeVC

reduction p q = toPTree $ filterOut (flip (comparing (mquestToQuest q)) (getCarpetW False q)) (snd p)


safeDestroy :: PTreeVC -> MQuest -> Maybe PTreeVC
safeDestroy p q = fmap toPTree $ replaceM (\t -> stdMaybe (comparing (mquestToQuest q) (getLeaf t)  (getCarpetW False q)) (empty)) (fst p) (snd p)

destroy :: PTreeVC -> MQuest ~~> PTreeVC
destroy pt = bAction (safeDestroy pt)

reduce :: PTreeVC -> MQuest ~-~> PTreeVC
reduce pt= bAction (toW . pure .  reduction pt)

fin :: W PTreeVC -> MQuest ~-~> MQuest
fin w = bAction (\q -> let
                                  (pt,mes) = runWriter w
                                  cell = getAim $ mquestToQuest q

                                  b2 = isConstMap $ snd $ cell
                                  edge = (fst cell, aimFromCell cell)
                                  path = concat $ maybeToList (fmap (findRoute edge) $ mes)
                                  --b =  isWDMap $ snd $ cell
                                  end = id -- stdEndo b (addMapToMQuest edge (project $ snd cell, path)) 
                                  addTheConstructedPath =  stdEndo b2 (addMapToMQuest edge (project $ snd cell, path))                                 
                               in bAction $  sequenceA $ writer (fmap (end . addTheConstructedPath) . action (endProof pt) $ q, mes))
                               
toW :: (Monad g, Functor f) => f a -> (f :*: g) a
toW = bAction . fmap return
swapSides :: MQuest -> MQuest
swapSides = qmap (flipP')

nn :: Int -> (Int,Int)
nn leng = (leng,leng*leng)
line = ((map Info [(Ker, Hori), (Im, Hori)],nn 3),([],[0..2])) :: WorldInfo
{--exactReDef =swapSides `fmap` exactDef
exactDef = Just  . mquestToMQuest . questToMQuest $ (line, [(1, (Ker, Hori) --> (Im, Hori)), (1, (Im, Hori) --> (Ker, Hori)), (1,Exact Hori)]) :: Maybe MQuest
--}
--mapdef = Just . mquestToQuest. questToMQuest $ (line, [(0,Map 1 (Empty Verti)), (0, Map 1 (WellDefined Verti)), ()])
shrinkAssumptions :: MQuest -> MQuest
shrinkAssumptions = qmap (\ (P (a,b)) -> P (shrinkGump a , b)) --shrinkGump
reformAims :: MQuest -> MQuest
reformAims =  (qmap (fmapP (extendAll . shrinkGump)))
--shrinkAtPos pos =fromJust . fmap swapSides . action (replaceMQuest ([0],pos) exactReDef). swapSides
addAss :: Cell -> MQuest -> MQuest
addAss c= shrinkAssumptions . qmap (addAssCore c)
addAssStrict c= stdEndo (isAssumptionable (snd c)) $ addAss c

addAssCore :: Cell -> PropCell -> PropCell
addAssCore c (P (ass,ai)) = P((addCell ass c), ai)
addMapToMQuest :: Edge -> (Dir,[Int]) -> MQuest -> MQuest
addMapToMQuest e ls = let func = \q  -> MQuest (addMapsToWorldInfo . mquestToQuest $ q, snd $ mquest $ q)
                      in func .  addAss (fst e, (Defin ls)) --Map (snd e) 


getAim :: Quest -> Cell
getAim = last' . snd
getAimSafe :: Quest -> Maybe Cell
getAimSafe = safelast . snd
eA :: Prop Cell -> Prop Cell
eA pc = (elimA (getFirstLeafPath (ai' pc)) pc) 
                  
                  
killEmptyParents :: Prop Cell -> Prop Cell
killEmptyParents pc = if 
					fmap (isConcatenated . snd) (safehead . gumpToList . ai' $ pc) == Just True 
					then  eA pc else pc

elimAction :: Prop Cell -> Prop Cell
elimAction pc =  tillTerminate killEmptyParents $ eA pc --  if (isConstMap $ snd $ headAI' pc) then replaceStd (0 ::Int) (newcell pc) pc else 
                
endProof :: PTreeVC -> MQuest ~~> MQuest
endProof pt = let
--                  newcell = (insertLeft)  . fmap ( \((Defin (h,_))) -> Map i (WellDefined h)). headAI'  :: PropCell -> PropCell-- pc = let (Map i (Empty h)) = snd $ cell pc in [(fst cell, Map i (WellDefined h))]
                  
                  
              in bAction (\q ->(stdMaybe (null . allLeafs . snd . reduction pt $ q) q) 
				<&> maybePt addAssEndo 
				<&> qmap (elimAction))

addAssEndo :: MQuest -> Maybe MQuest
addAssEndo q = do
					aim <- getAimSafe =<< mquestToQuestSafe q
					return $ addAssStrict aim q  --if ((isConstMap . snd) aim) then return q else 

assoc :: Functor f => (f :*: (g :*: h)) b -> ((f :*: g) :*: h) b
assoc = bAction . bAction . fmap action . action
assoc2 :: Functor f => ((f :*: g) :*: h) b -> (f :*: (g :*: h)) b
assoc2 =bAction . fmap bAction . action . action
{--
(<§>) :: a -> (a -> b) -> b
infixl 0 <§>
(<§>) = flip ($)
--}
------------------------------------------------------------------------------------------------------

type a :*: b = Compose a b

(||>) ::Maybe w -> (w ~-~> a) -> Maybe a
infixl 0 ||>
w ||> x = fmap (fst . runWriter) (w |||> x)
(|||>) :: Maybe w -> (w ~-~> a) -> Maybe (W a)
infixl 0 |||>
w |||> (x) = w >>= (action . action x)
infixl 0 >|>
(>|>) :: Maybe MQuest -> Maybe MQuest -> Maybe MQuest
a >|> b = a >>= action (replaceMQuest ([0],[[0]]) b) --let
            --    [a,b] = (map (fmap (fst . runWriter) . action) [a',b']) :: [Maybe MQuest]

             --in toW (a >>= action (replaceMQuest (0,[0]) b))
infixl 0 >||>              
(>||>) :: Maybe MQuest ->  Maybe MQuest   -> Maybe MQuest
a >||> b = a >>= action (replaceMQuest ([0],[getFirstLeafPath (ai' $ getProp $ a) ]) b) --let
{--
(°) :: Monad m => (((->) a) :*: m) b -> (((->) b) :*: m) c -> (((->) a) :*: m) c
(°) f g = bAction $ action f >=> action g
--}


--infixl 9 °
transformMQuest :: MQuest -> Transformation -> MQuest
transformMQuest (MQuest (w, p)) c = MQuest(w,transformSndPartOfQuest p c )
newtype MQuest = MQuest {mquest :: (WorldInfo,PropCell)} deriving Eq
instance Show MQuest where
  show = showMQuest

questToMQuest :: Quest -> MQuest
questToMQuest (i,arr) = reformAims $ sMQuest (i, P (listToGump (init' arr), return (last' arr)))
showQuest = showMQuest . questToMQuest
showMQuest :: MQuest -> String
showMQuest mq = let v = vg' mq in  show v ++ "\n" ++ "Assumptions: \n" ++ showCells v False q2 ++ "\n\n" ++ "Remaining Aims: \n" ++ showCells v True q3 where
	(MQuest (q1,P (q2,q3))) = mq
type Transformation = (Int -> Int,Bool)
type Cell = (Int,L)
transformL' :: Transformation -> L -> L
transformL' (f,b) (Map i (Defin (x, l))) = Map (f i) (transformL b (Defin (x, map f l)))
transformL' (f,b) x = transformL b x
transformCell:: Transformation -> Cell -> Cell
transformCell (f,b) (i,l) = (f i, transformL' (f,b) l)
transformSndPartOfQuest :: PropCell -> Transformation -> PropCell
transformSndPartOfQuest (P (p,q)) c = P (fmap (transformCell c ) p, fmap (transformCell c ) q)
constructTrafo :: Quest -> Quest -> Transformation
constructTrafo (w0,s0) (w1,s1) = let
                                        [l0,l1] = map getLenFromWorldInfo [w0,w1]
                                        [s0',s1'] = map last' [s0,s1]
                                        dire = (\s -> project $ snd s) :: Cell -> Dir
                                        b1 = dire s0' == dire s1'
                                        --b = if (b1 && dire s0' == Diag) then not $ s0 `subList` s1 else False
                                        b
                                          | (not b1) = True
                                          | (dire s0' == Diag) = not $ s0 `subList` s1
                                          | otherwise = False
                                        func q0 = let
                                                      [p0,p1] = map fst [s0',s1']
                                                      qp0 = q0 - p0
                                                      x0 = qp0 `mod` l0 - if (q0 `mod` l0 < p0 `mod` l0) then l0 else 0
                                                      y0 = (qp0 - x0) `div` l0
                                                      [x1,y1] = (if b then reverse else id) [x0,y0]
                                                  in x1 + l1 * y1 + p1
                                  in (func,b)



qmap' :: (MQuest -> PropCell) -> MQuest -> MQuest
qmap' f q = MQuest (fst $ mquest q, f q)

sndEater :: (Monad g, Traversable g, Applicative f) => (a -> (f :*: g) b) -> g a -> (f :*: g) b
sndEater f = bAction . fmap (join) . action . traverse f
fstEater :: (Monad g, Traversable g, Monad f) => (a -> (f :*: g) b) -> f a -> (f :*: g) b

fstEater phi = bAction . join . fmap action . fmap phi

replaceMQuest :: (Path,[Path]) -> Maybe MQuest -> MQuest ~~> MQuest
replaceMQuest arr c = bAction (fmap (qmap killEmptyParents) . f )  where
	replaceMQuestINTERMED :: [(Path,Path)] -> Maybe MQuest -> MQuest ~~> MQuest
	replaceMQuestINTERMED is = sndEater (\q -> bAction (func is q ))
	replaceQuest = replaceMQuestINTERMED . sequenceA
	f = action (replaceQuest arr c) :: MQuest -> Maybe MQuest
    
                     --}
func :: [(Path , Path)] -> MQuest -> MQuest -> Maybe MQuest
func is q s = foldM (\bo i -> replaceJo i q bo) s is 

isDirectAim :: L -> Bool
isDirectAim (Map i (Defin (_,_))) = True
isDirectAim _ = False -- TODO

replaceJo :: (Path,Path) -> MQuest -> MQuest -> Maybe MQuest
replaceJo (i,j) q big  =  let
                              [p,a] = map (snd . mquest ) [q,big] :: [PropCell]
                              [p',a'] = zipWith mquestToQuest' [i,j] [q,big]
                              tr = constructTrafo p' a'
                              q' = (transformSndPartOfQuest p tr)
                              aim = val $ (undForest $ ai' a) !! 0
                              ff = (safeReplaceA (==) j q' $ stdEndo (isDirectAim $ snd aim) (addAssCore aim) a) :: Maybe (PropCell)
                              wi = (fst . mquest $ big) 
                              imp = ((elimAll (==) . pmap filterDs) `fmap` ff) 
                          in sMQuest `fmap` sequenceA (wi, imp) --
getProp :: Maybe MQuest -> PropCell
getProp (Just (MQuest (_, z))) = z
goAi :: Maybe MQuest -> Path -> Cell
goAi m p = (ai' $ getProp m) <!!> p 
elimWriter = fst . runWriter
iwf :: (MQuest ~-~> a) -> (W MQuest ~-~> a)
iwf = (cofmap elimWriter)
(>>~) :: (a ~-~> b) ->  (W b -> a ~-~> c) -> (a ~-~> c)
infixl 1 >>~
x >>~ f = assoc2 . fstEater (assoc . f) . bAction . fmap action . action $ x
introTree :: P [] (Cell , Maybe Cell) -> P Gump Cell
introTree ( P (a,b)) = P (tableToGump a , extendAll $ tableToGump b)
introMQuest :: (WorldInfo, P [] (Cell , Maybe Cell)) -> MQuest
introMQuest =  MQuest . fmap introTree 

