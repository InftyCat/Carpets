{-# LANGUAGE DeriveFoldable, FlexibleInstances #-}
module Application.Logic.CarpetTrees3 where
import Application.Logic.Graph4
import Application.Logic.BasicFunctions
import Application.Logic.Carpets4
import Control.Monad
import Data.Functor
import ClassyPrelude hiding (intersection)
import Data.List ((!!))
import Data.Foldable (foldl)
import Data.Tree
import Application.Logic.Tre
import Application.Logic.Term2
import Application.Logic.MappingTree
import Control.Monad.Writer
import Data.Maybe
import Data.Functor.Compose

type TreF f a = Tre a (f a)
type TreeVC = TreF Sta VGraph

reduceEither :: Either a a -> a
reduceEither (Left a) = a
reduceEither (Right a) = a
root :: TreeVC -> VGraph
root (Tre t) = let Just x = rootLabel t in reduceEither $ fmap (snd . sta) x


type PTreeVC = PTre VGraph Carpet


-- changes the TreeVC at the given position by evaluating the given function
--replaceM :: (TreeVC -> Maybe TreeVC) -> Path -> TreeVC -> Maybe TreeVC




getCommonEdges :: VGraph -> VGraph -> VGraph -> [Edge]
getCommonEdges r g g' = (getAllEdges r) `intersection` (getAllEdges g ++ getAllEdges g')

rMap ::  Functor f => (a -> a) -> Tre a (f a) -> Tre a (f a)
rMap = treMap . changeRootLabel . fmap. fmap . fmap
nodevgts  :: TreeVC -> Maybe (VGraph, TreeVC, TreeVC)
nodevgts (Tre tr) = stdMaybe (2 <= (length $ subForest tr)) (let (t:s:xs) = subForest tr in (root (Tre tr),Tre t,Tre s))
twoOutOfThree :: TreeVC -> Maybe TreeVC
twoOutOfThree = fmap (\(vg,t,s) -> let
                                                edges = getCommonEdges vg (root t) (root s)
                                                f ts = foldr (addEdgeV') ts edges
                                             in node vg $ map (rMap f) [t,s]) . nodevgts

traverse' :: (Traversable t, Applicative m) => (t b -> t (t b)) -> (t b -> m b) -> t b -> m (t b)
traverse' j f k = traverse f (j k)

ret :: Applicative m => ((a, b) -> m b) -> (a, b) -> m (a, b)
ret = traverse' repl


--wrap :: (x -> Maybe y) -> (x -> y)
--wrap f x = y where Just y = f x
tOT' :: PTreeVC -> Maybe PTreeVC
tOT' = ret (\(p,t) -> replaceM (twoOutOfThree) (init' p) t)

node2 :: VGraph -> Carpet -> Carpet -> TreeVC
node2 cvg c1 c2 = node cvg $ map (leaf) [c1,c2]
split :: World -> [Int] -> Edge -> Carpet -> Maybe TreeVC
split w n e c = let
                      elim = filter (/= (snd e))
                      cvg =(snd $ carpet c)
                      inducedCarpet' l c' = let
                                                (Sta (b,vg)) = c'
                                                vg'' = addZeroEdges w (elim l) b vg in Sta (b,vg'')
                      c2 = fmap (inducedCarpet' n ) $ (overwrite w e) (inducedCarpet w c n) :: Maybe Carpet
                      c1 = addWeightedNode (snd e) one $ (inducedCarpet w c ((nodes cvg) `comp` (elim n))) :: Carpet
                in fmap (node2 cvg c1) c2



deleteNodeInHistory :: Int -> PTreeVC ->Maybe TreeVC
deleteNodeInHistory i = uncurry (deleteNodeInHistory' i)
deleteNodeInHistory' :: Int -> Path -> TreeVC -> Maybe TreeVC
deleteNodeInHistory' i par = flip (foldl (\a p -> a >>= replaceM' p)) ances . Just where
                                  ances = getAncestors par -- foldl (\t a -> replaceM)
                                  replaceM' = (\p' -> replaceM (Just . rMap (deleteNode i )) p') :: Path -> TreeVC -> Maybe TreeVC
getAncestors :: [a] -> [[a]]
getAncestors [] = [[]]
getAncestors (x:xs) = [] : (map (x:) $ getAncestors xs)


safeLaserAction :: World -> Edge -> PTreeVC -> MW PTreeVC
safeLaserAction w e=  Compose . fmap return . ret (leafAction (safeLaser w e)) --addEdgeAction e

--addZeros :: World -> Edge -> PTreeVC -> Maybe PTreeVC
--addZeros wld (v,w) = ret$ leafAction (fmap Sta .  sequenceA . \(Sta (b,vg)) -> (b,add0Edge wld b (v,w) vg))
(>==>) :: (PTreeVC -> Maybe PTreeVC) -> (PTreeVC -> Maybe TreeVC) -> (PTreeVC -> Maybe PTreeVC)
f >==> g = (f >=> (ret g))

outerLaserAction :: (Int,World) -> Edge -> PTreeVC -> MW PTreeVC
outerLaserAction (i,w) e = (ovAction (i,w) e <=< safeLaserAction w e)

ovAction :: (Int,World) -> Edge -> PTreeVC -> MW PTreeVC
ovAction (i,w) e =  let
                        dNinHist =flip fromMaybe =<< (ret (leafAction (\c -> stdM (not $ whenToDeleteNode True w (fst $ sta c) e) (pure Nothing) c)) >==> (deleteNodeInHistory (snd e)))  -- (\pt -> stdM (whenToDeleteNode True w (fst $ carpet $ headCarpet pt) e) (ret (deleteNodeInHistory (snd e))) pt) --
                    in addEdgeAction e . (ret (leafAction (overwriteAble w i e)) >=> pure . dNinHist >==> leafAction (overwrite w e))   -- overwriteAble überarbeiten!
addEdgeAction :: Edge -> Maybe a -> MW a
addEdgeAction e= Compose . fmap (\p -> writer (p,edgeToTree e))

splitAction :: World -> Edge -> [Int] -> PTreeVC -> MW PTreeVC
splitAction w e l =         let
                                    g = (addEdgeAction e . leafGAction (split w l e)) :: PTreeVC -> MW TreeVC
                                    h = (\p t -> ((0:p),t)) ::  Path -> TreeVC -> PTreeVC
                                  in liftM2 fmap (h . fst) (g) -- \pt -> fmap (h $ fst pt) (g pt)

choiceAction :: World -> Edge -> PTreeVC -> MW PTreeVC
choiceAction w e = Compose . fmap return . ret (leafAction(addChoices w e))

addChoices :: World -> Edge -> Carpet -> Maybe Carpet
addChoices w e c = let 
	(b,vg) = carpet c in 
	do
		ei <- getEdgeMapInformation vg e
		fmap (fmap (addEdgeV' e)) $ addEdgePathCareLess w (getEdgePath ei) c >>= 
			stdMaybe (b <!> (fst e) <= std (getDim b) one)

type MW = Compose Maybe W
universal :: (Int,World) -> Edge -> PTreeVC -> MW PTreeVC
universal (i,w) e p
                      | (not (isAddable w b e)) && isLaserable w (b,vg) e = outerLaserAction (i,w) e p
                      | (isAddable w b e && not (isLaserable w (b,vg) e)) = ovAction (i,w) e p
                      | isLaserable w (b,vg) e = safeLaserAction w e p
                      | otherwise = splitAction w e [snd e] p
                      where (Sta (b,vg)) = headCarpet p


apL' f arr = flip (foldl (\a qr -> a >>= f qr)) arr . return
apL :: Monad m => (Edge -> PTreeVC -> m PTreeVC ) -> [Int] -> PTreeVC -> m PTreeVC
apL f = apL' f . toEdges
apLM :: Monad m => (Edge -> PTreeVC -> m PTreeVC ) -> [[Int]] -> PTreeVC -> m PTreeVC
apLMKAPUTT f = foldl (>=>) pure . map (apL f)
apLM f arr = apL' f (arr >>= toEdges)
----------------------------

headCarpet :: PTreeVC -> Carpet
headCarpet (p,t) = getLeaf $ go t p
{--
swapFocus :: Path -> Path
swapFocus arr
              | x == 0 = y ++ [1]
              | x== 1 = swapFocus y
              where x = last arr
                    y = init' arr
--}
switch :: PTreeVC -> PTreeVC
switch (p,t)= (next (allLeafPaths t) p, t) --(swapFocus p,t)


infixr 1 >==>

{--(§) :: Functor f => f a -> (a -> b) -> f b
x § b = fmap b x
REPLACED BY <&>
infixl 1 §
Lösungen:


--}
