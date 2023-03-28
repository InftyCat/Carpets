module Web.Controller.Cells where

import Web.Controller.Prelude
import Web.View.Cells.Index
import Web.View.Cells.New
import Web.View.Cells.Edit
import Web.View.Cells.Show
import Application.Logic.CarpetCentral4 hiding ((|>) , Cell)
import Application.Logic.Carpets4 hiding (World)
import Application.Logic.Graph4
import Application.Logic.Worlds
import Application.Logic.BasicFunctions
import Data.List.Split (wordsBy)
import Data.Text (unpack)
instance Controller CellsController where

    action CellsAction = do
        cells <- query @Cell |> fetch
        render IndexView { .. }

    action NewCellAction { questId }= do
        let cell = newRecord
	        |> set #questId questId
        quest <- fetch questId
        render NewView { .. }

    action ShowCellAction { cellId } = do
        cell <- fetch cellId
        render ShowView { .. }

    action EditCellAction { cellId } = do
        cell <- fetch cellId
        render EditView { .. }

    action UpdateCellAction { cellId } = do
        cell <- fetch cellId
        cell
            |> buildCell
            |> ifValid \case
                Left cell -> render EditView { .. }
                Right cell -> do
                    cell <- cell |> updateRecord
                    setSuccessMessage "Cell updated"
                    redirectTo EditCellAction { .. }
	--action copyCelAction { cellId , 
    action CreateCellAction = do
        let cell = newRecord @Cell
        cell
            |> buildCell
            |> ifValid \case
                Left cell ->  do
                	quest <- fetch cell.questId
                	render NewView { .. } 
                Right cell -> do
                    cell <- cell |> createRecord
                    setSuccessMessage "Cell created"
                    redirectTo ShowQuestAction { questId = cell.questId }
   
    action CopyMultipleCellsAction { oldQuestId , newQuestId } = do
		oldQuest <- fetch oldQuestId
				 >>= fetchRelated #cells
		let newCells = map (\o -> newRecord @Cell
                      |> copyFields @["pos","assumption","definArr","info","direction"] o
                      |> set #questId newQuestId |> buildCell |> createRecord) oldQuest.cells
		blahs <- sequence newCells
		redirectTo ShowQuestAction { questId = newQuestId }
		--
		--redirectTo ((CreateSessionAction (newQuestId))) -- CopyCellAction { cellId = "d367d09e-8bb5-4ee5-805b-4a9c7ea24f96" , newQuestId = newQuestId }-- CellsAction -- (CreateSessionAction newQuestId)
    action DeleteCellAction { cellId } = do
        cell <- fetch cellId
        deleteRecord cell
        setSuccessMessage "Cell deleted"
        redirectTo CellsAction

buildCell cell = cell
    |> fill @["questId", "pos", "direction", "assumption" , "info","definArr"]

buildLogicCell :: Cell -> LogicalCell   
buildLogicCell c = (c.pos , infoToLogicalInfo ((map unpack $ c.info))  (map toDir $ (map unpack $ c.direction)) (c.definArr))
 

