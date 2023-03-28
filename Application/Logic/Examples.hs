{-# LANGUAGE TypeOperators #-}
-- :set -package mtl
-- :set -package split
--{-# set -package split #-}
module Application.Logic.Examples where
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
import Application.Logic.Worlds
import Application.Logic.TreePropositions
import Application.Logic.Quests
import IHP.Prelude hiding ((|>))


tc3InjSol = tc3Inj ||> start >>= uni [[0,3,4],[0,1,4,3,0]] >>~ fin



{--sol satisfies
sol x >>= (x |>) . fin
x |> sol' x >>~ fin
--}

sol :: Maybe MQuest -> Maybe PTreeVC
sol x = x ||> sol' x
sol' x
        | x == epiIntro =     start >>= out [1,3,0] >>= spl (0,1) [1] >>= tOT >>= arg [3,1]
        | x ==  tc3Surj =     start >>= out [2,5,4,7] >>= out [5,8] >>= arg [8,7] >>= out [7,6] >>= out [6,3] >>= spl (3,4) [2,4,5] >>= tOT >>= arg [7,4] >>= out [4,1] >>= spl (1,2) [2] >>= tOT >>= arg [5,2]
        | x ==  viererSurj =  start >>= out [5,6,2,3] >>= out [6,7] >>= arg [7,3,2] >>= out [2,1] >>= spl (1,5) [5] >>= tOT >>= arg [6,5] >>= out [5,4,0] >>= edg [0,1] >>= arg [1,5]
        | x ==  viererInj =   start >>= out [2,3] >>= out [2,6,7] >>= arg [7,3,2] >>= out [2,1,5] >>= arg [6,5] >>= out [5,4,0] >>= spl (0,1) [1] >>= tOT >>= arg [5,1] >>= edg [1,2] >>= tOT <&> switch >>= arg [1,2]
        | x == tc3Inj = 	  start >>= uni [[0,3,4],[0,1,4,3,0]]
        | x ==  tc3Ex1 =      start >>= out [1,0,3,4,5] >>= out [1,2] >>= arg [5,2,1]
        | x ==  tc3Ex2 =      start >>= out [1,2,5] >>= out [1,4] >>= arg [5,4] >>= out [4,3,6] >>= out [4,7] >>= arg [7,6,3] >>= out [3,0] >>= spl (0,1) [1] >>= tOT >>= arg [4,1] >>= tOT <&> switch >>= reduce >>= spl (1,2) [2] >>= tOT <&> switch >>= tOT >>= uni [[5,2,1]]
        --start >>= out [1,2,5] >>= out [1,4] >>= arg [5,4] >>= out [4,3,6] >>= out [4,7] >>= arg [7,6,3] >>= out [3,0] >>= spl (0,1) [1] >>= tOT >>= arg [4,1] >>= tOT <&> switch >>= arg [2,1]
        | x == lars    =      start >>= uni [[6,3,4],[6,7,4,5], [4,1,2], [5,2,1,0]] >>= spl (0,3) [3,6] >>= tOT >>= arg [4,3,6]
        | x == injIntro = 	  start >>= out [0,1,3] >>= arg [3,0] 
        | x == injComp = 	  start >>= uni [[0,1,3,0]]

sol'' x
        | x == viererInj =     start >>= uni [[2,3],[2,6,7,3,2,1,5],[6,5,4,0]] >>= spl (0,1) [1,2] >>= tOT >>= uni [[5,1,2]]
erg = viererSurj >|> epiIntro ||> start >>= out [5,4,0,1] >>= arg [1,5] >>~ fin ||> start >>= out [6,7] >>= out [6,2,3] >>= arg [7,3,2] >>= out [2,1] >>= arg [1,6] >>~ fin
viererFast = viererSurj >|> epiIntro 
	||> start  >>= uni [[5,4,0,1,5]] >>~ fin 
	||> start >>= uni [[6,2],[6,7,3,2,1,5]] >>~ fin
erg2 = tc3Surj 
	>|> epiIntro 
	||> start >>= out [2,5] >>= arg [5,2] >>~ fin 
	||> start >>= out [5,4,7] >>= out [5,8] >>= arg [8,7] >>= out [7,6,3] >>= spl (3,4) [4,2,5] >>= tOT >>= arg [7,4] >>= out [4,1] >>= arg [1,5] >>~ fin
erg3 = tc3Inj >|> injIntro >|> injComp
interMedKerFac = kerFactorization ||> start >>= out [0,1,4] >>= out [1,2,5] >>= arg [5,4] >>= out [4,3] >>~ fin
ergKerFac = kerFactorization 
	||> start >>= out [0,1,4] >>= out [1,2,5] >>= arg [5,4] >>= out [4,3] >>~ fin 
	||> start >>= choice (0,3) >>= arg [0,1,4,3] >>~ fin
ergCoKerFac = cokerFactorization 
	||> start >>= out [2,1,4,5] >>~ fin 
	||> start >>= arg [2,1] >>= out [1,0,3] >>= arg [3,4,5] >>~ fin
ergSnakeExact =      snakeEx 
	||> start >>= choice (1,2) >>= arg [1,2,5,4] >>= out [4,3,6] >>= uni [[4,7,6,3,0]] >>= spl (0,1) [1] >>= tOT >>= arg [4,1] >>= reduce >>= spl (1,2) [2] >>= tOT >>= arg [5,2] >>= reduce >>= arg [5,2,1] >>~ fin
	||> start >>= uni [[1,0,3,4,5],[1,2],[5,2,1]] >>~ fin


	||> start >>= out [2,9,10] >>= choice (2,9) >>= arg [9,6] >>= out [6,3]
                          >>= spl (3,4) [4,5,2,9] >>= tOT >>= arg [7,4] >>= out [4,1] >>= reduce
                          >>= spl (1,2) [2] >>= tOT >>= arg [5,2] >>= edg [2,9] >>= tOT >>= reduce >>= arg [9,2] >>~ fin                          
    ||> start >>= out [2,1] >>= choice (1,2) >>= choice (2,9) >>= arg [9,2] >>~ fin
    --note that by the last version of snakeEx the remaining quest is not possible
ergSnake' =  snake 
	>|> kerFactorization 
	>|> kerFactorization
	||> start >>= uni [[2,5,4,7],[5,8,7,6,9]] >>~ fin
	||> start >>= choice (2,9) >>= uni [[2,5,4,3]] >>= spl (3,6) [6,9] >>= tOT >>= uni [[7,6]] >>=  arg [6,9] >>~ fin -- inserting "reduce >->" before "arg [6,9]" yields an exception
	>|> cokerFactorization 
	>|> cokerFactorization 
                    --  ergSnake' ||> start >>= out [2,9,10] >>= choice (2,9) >>= arg [9,6] >>= out [6,3] >>= spl (3,4) [4,5,2,9] >>= tOT >>= arg [7,4] >>= out [4,1] >>= reduce >>= spl (1,2) [2] >>= tOT >>= arg [5,2] >>= edg [2,9] >>= tOT >>= reduce >>= arg [9,2] >>~ fin ||> start >>= out [2,1] >>= choice (1,2) >>= choice (2,9) >>= arg [9,2] >>~ fin

--pT = printMT . snd . runWriter . fromJust :: Maybe (W a) -> IO ()
checkSols x =  x ||> sol' x >>~ fin
checkAllSols = mapM checkSols . map (allQuests !! ) $ ([0..9] :: [Int])
--snake |> start >>= out [1,4,5] >>= out [4,7,8] >>= arg [8,5] >>= out [5,2] >>~ fin ||> startWD >>=> arg [1,4] >-> arg [4,5,2] >>~ henceWellDefined

-- ergSnake ||> start >>= out [2,9,10] >>= choice (2,9) >>= arg [9,6] >>= out [6,3] >>= spl (3,4) [4,5,2] >>= tOT >>= arg [7,4] >>= out [4,1] >>= reduce >>= spl (1,2) [2]
ergExactIntro = exactIntro ||> start >>= uni [[4,1,2],[4,5,2,1,0,3,4]] >>~ fin ||> start >>= uni [[4,3,0,1,2,5,4]] >>~ fin
tc3Sol = tc3 >|> tc3Inj >||> tc3Ex2 >||> tc3Ex1 >|> tc3Surj


