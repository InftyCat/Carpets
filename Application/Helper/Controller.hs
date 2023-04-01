module Application.Helper.Controller where

import IHP.ControllerPrelude

-- Here you can add functions which are available in all your controllers

import Application.Logic.CarpetCentral4 hiding ((|>))
import Application.Logic.Carpets4 hiding (World)
import Application.Logic.Graph4
import Application.Logic.Worlds
import Application.Logic.BasicFunctions
--import Web.Controller.Worlds
type LogicalCell = Application.Logic.CarpetCentral4.Cell


infoToLogicalInfo :: [String] -> [Dir] -> [Int] -> L
infoToLogicalInfo ["WellDefined"] [d] [x] =  WellDefined d x
infoToLogicalInfo ["Defin"] [d] arr = Defin (d , arr)
infoToLogicalInfo [x1 , x2] [d1 , d2] [] = I (Info (toInfoC x1 , d1)) (Info (toInfoC x2 , d2))
infoToLogicalInfo ["Epi"] [d] [] = Epi d
infoToLogicalInfo ["Mono"] [d] [] = Mono d
infoToLogicalInfo ["Exact"] [d] [] = Exact d
infoToLogicalInfo ["Map"] [d] arr = Map (last' arr) (Defin (d , arr))
toInfoC :: String -> InfoC
toInfoC "Ker" = Ker
toInfoC "Im" = Im
toDir :: String -> Dir
toDir "Hori" = Hori
toDir "Verti" = Verti
toDir "Diag" = Diag
