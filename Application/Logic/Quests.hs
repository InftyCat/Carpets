module Application.Logic.Quests where
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
import Data.List (drop , take)
import IHP.Prelude
-----


introMQuest' :: (WorldInfo , P [] Cell) -> MQuest
introMQuest' = introMQuest . fmap (fmap (\x -> (x , Nothing)))

allQuests' = [
                                         (triangle',  [(0,Mono Diag),
                                                            (0,Mono Hori)]) :: Quest,
                                         (triangle,
                                                                      [(1, (Ker, Verti)  --> (Im,Hori)),(3, (Im, Verti) -->  (Im, Diag)),
                                                                      (1,Epi Hori)]) :: Quest,
                                        (triangle, [(0, Mono Hori), (1, Mono Verti), (0, Mono Diag)]) :: Quest,
                                        (square,
                                                                addExacts [(4, Hori), (7, Hori),(4,Verti),(5,Verti)] ++ [(2, Mono Verti), (5,Epi Hori), (6, Epi Verti),
                                                                (2,Epi Hori)]) :: Quest,
                                         (square,
                                                                addExacts [(4, Hori)] ++ [(2, Mono Verti),
                                                                (1,(Im , Hori) --> (Ker , Hori))]) :: Quest,
                                        (square,
                                                                addExacts [(4, Hori),(4,Verti),(3,Verti)] ++ [(6, Mono Hori),(1,Mono Verti), (2,Mono Verti),
                                                                (1,(Ker , Hori) --> (Im , Hori))]) :: Quest,
                                        (square,
                                                              [(0, Mono Verti), (3, Mono Hori), (0, Mono Hori)]) :: Quest,
                                         (rect,
                                                              addMEx (Hori, [1,2,5,6]) ++ [(6, Epi Verti), (3, Mono Verti), (4, Epi Verti),
                                                              (5,Epi Verti)]) :: Quest,
                                         (rect,
                                                            addMEx (Hori,[2,1,5,6]) ++ [(1, Mono Verti), (3, Mono Verti), (4, Epi Verti), --(2, (Ker , Hori) --> (Im , Hori)),
                                                                (2,Mono Verti)]) :: Quest,
                                         (square',
                                                            addExacts [(1,Hori), (4,Verti)] ++ [(4, nullComp Hori), (3, nullComp Verti), (3, Mono Hori), (2, Mono Verti), (6, Epi Verti), (6, Mono Hori)]) :: Quest
                                                                ]

nullComp :: Dir -> Jo Info b
nullComp d = (Im, d) --> (Ker, d)
allQuests = (map (Just  . mquestToMQuest . questToMQuest) allQuests') :: [Maybe MQuest]
allMQuests = map (Just . mquestToMQuest . introMQuest') [
                                                    (squareBig,P (
													 addMEx (Hori,[4,7]) ++ addMEx (Verti, [3,4,5,6,7,8]) ++ [(9, Epi Verti), (10, Epi Verti), (11 , Epi Verti) , (5, Epi Hori), (6, Mono Hori), (2, Mono Verti), (1, Mono Verti), (0, Mono Verti)]
														,
														[(0, Map 1 (Defin (Hori, [0,3,4,1]))),
														 (1, Map 2 (Defin (Hori, [1,4,5,2]))),
														 (2, Map 9 (Defin (Hori, [2,5,4,7,6,9]))), 
														 (9 ,Map 10(Defin (Hori, [9,6,7,10]))), 
														 (10,Map 11(Defin (Hori, [10,7,8,11])))])),
                                                    (squareBig,P (addMEx (Hori,[4,7]) ++ addMEx (Verti, [3,4,5,6]) ++ [(5, Epi Hori), (6, Mono Hori), (2, Mono Verti), (1, Mono Verti), (0, Mono Verti),
                                                    (0, Map 1 (Defin (Hori, [0,3,4,1]))),(1, Map 2 (Defin (Hori, [1,4,5,2]))),(2, Map 9 (Defin (Hori,[2,5,4,7,6,9]))), (9,Map 10 (Defin (Hori, [9,6,7,10])))],[(1, Exact Hori), (2, Exact Hori) , (9, Exact Hori) ])),
                                                    (rectSmall, P (addMEx (Hori, [1,4]) ++ [(0, Mono Hori),(3, Mono Hori)],
                                                                  [(0,Map 3 (Defin (Verti,[0,1,4,3])) )])),
                                                    (rectSmall, P (addMEx (Hori, [1,4]) ++ [(2, Epi Hori),(5, Epi Hori)],
                                                                  [(2,Map 5 (Defin (Verti,[2,1,4,5])) )])),
                                                    (rectSmall, P (  [(1, Exact Hori),(1, Exact Hori), (3, Epi Verti), (4, Epi Verti),(2, Mono Verti)],[ (4, Exact Hori)]))
                                                    ]
[snake, snakeEx, kerFactorization, cokerFactorization, exactIntro] = allMQuests

tc3 = (Just $ mquestToMQuest $ addQuests (take 4 $ drop 3 allQuests')) -- >>= action (replaceMQuest ([0],map return [1,2]) exactReDef)
[injIntro,epiIntro,injComp,tc3Surj,tc3Ex1,tc3Ex2,tc3Inj, viererSurj,viererInj, lars] = allQuests
