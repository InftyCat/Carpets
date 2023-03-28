module Web.View.Quests.Show where
import Web.View.Prelude
import Application.Logic.CarpetCentral4 hiding ((|>) , Quest , Cell)
import Application.Logic.Propositions3
import Application.Logic.BasicFunctions
import Application.Logic.TreePropositions

import Application.Helper.Controller
import Web.Controller.Worlds
import Web.Controller.Cells
import Data.Text (pack, unpack)
import Text.Read

buildPropCell :: [Cell] -> PropCell
buildPropCell cs = let 
	(c1 , c2) = twoBins (not . snd) $ map (\c -> ((unpackId c.id , c.parentId), c.assumption)) cs
	func :: UUID -> LogicalCell
	func id = buildLogicCell $ head' $ filter (\c -> unpackId c.id == id) cs
	act = map ( (\(x,y) -> (func x , fmap func y)) . fst)
	in introTree $ P (act c1 , act c2)    



questToLogicQuest :: World -> [Cell] -> Maybe MQuest
questToLogicQuest w cs = Just  . MQuest $ (buildWorldInfo w , buildPropCell cs)
data ShowView = ShowView { world :: World , quest :: Include "cells" Quest }
c1 = "d367d09e-8bb5-4ee5-805b-4a9c7ea24f96" :: Id Cell 
c2 =  "42500fd2-9fd3-4355-83ef-9e8c56e46b04" :: Id Quest 
-- act2 = CopyCellAction {cellId = c1 , newQuestId = c2}
{--
act = CopyMultipleCellsAction {cellIds=[c1,c1] , 
	newQuestId=c2}
	--}
instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Viewing Quest {quest.name}</h1>
        <form action={ CopyQuestAction quest.id } method="post">
    		<button type="submit" name="your_name" value="your_value" class="btn-		link">Copy!
    		</button>
		</form>
		<form action={(CreateSessionAction quest.id)} method="post">
    		<button type="submit" name="your_name" value="your_value" class="btn-		link">Play!
    		</button>
		</form>
		
        <p>World : {quest.worldId}</p>
        <span style="white-space: pre-line">  {(questToLogicQuest world quest.cells)}{map (\c -> c.info) quest.cells}  </span>
	<a href={NewCellAction quest.id}> Add Cell </a>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Quests" QuestsAction
                            , breadcrumbText "Show Quest"
                            ]
--renderCell cell = [hsx|<div> {cell.direction} {cell.pos} {cell.info} </----div>|]                            
-- <div>{forEach quest.cells renderCell}</div>
                           
{--
CopyQuestAction quest.id (pack "COPY")
--}
