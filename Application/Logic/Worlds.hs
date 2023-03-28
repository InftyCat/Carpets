module Application.Logic.Worlds where
	import Application.Logic.CarpetCentral4
	import Application.Logic.CarpetTrees3
	import Application.Logic.Carpets4
	import Application.Logic.Propositions3
	import Data.Functor
	import Application.Logic.Term2
	import Application.Logic.Tre
	import Application.Logic.Graph4
	import Application.Logic.MappingTree
	import Control.Monad.Writer
	import Control.Arrow
	import Data.Functor.Compose
	import Data.Maybe
	import IHP.Prelude

	createPairs :: [Dir] -> [Info]
	createPairs arr = map Info (arr >>= (\x -> [(Ker , x), (Im , x)]))
	triangleOLD = ((map Info [(Ker, Verti), (Im, Verti),(Im, Hori),(Im, Diag)],nn 2),([],[0,1,3])) :: WorldInfo
	triangle = ((createPairs [Hori , Verti , Diag],nn 2),([],[0,1,3])) :: WorldInfo
	triangle' = ((createPairs [Hori, Verti, Diag],nn 2),([],[0,1,3])) :: WorldInfo
	square = ((createPairs [Hori, Verti, Diag],nn 3),([],[0..8])) :: WorldInfo
	square' = ((createPairs [Hori, Verti, Diag],nn 3),([],[0..7])) :: WorldInfo
	rect = ((createPairs [Hori, Verti],nn 4),([],[0..7])) :: WorldInfo
	squareBig = ((createPairs [Hori, Verti, Diag],(3,12)),([],[0..11])) :: WorldInfo
	rectSmall = ((createPairs [Verti, Hori],(3,6)),([],[0..5])) :: WorldInfo
