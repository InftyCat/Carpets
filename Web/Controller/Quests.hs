module Web.Controller.Quests where

import Web.Controller.Prelude
import Web.View.Quests.Index
import Web.View.Quests.New
import Web.View.Quests.Edit
import Web.View.Quests.Show


instance Controller QuestsController where
    action CopyQuestAction {questId} = do			
		oldQuest <- fetch questId
				 >>= fetchRelated #cells
		let quest = newRecord @Quest 
			|> copyFields @["name","worldId"] oldQuest
			|> set #name (oldQuest.name ++ "'") 
			|> set #org False 
			
		quest
				|> buildQuest
				|> ifValid \case
					Left quest -> do
						world <- fetch quest.worldId 
						render NewView { .. } 
					Right quest -> do
						quest <- quest |> createRecord                    
						setSuccessMessage "Quest copied"
						--redirectTo (foldM oldQuest.cells (\c -> CopyCellAction { cellId = c ,  newQuestId = quest.id}))
						redirectTo (CopyMultipleCellsAction { 
							oldQuestId = oldQuest.id, 
							newQuestId = quest.id})
						--

  
    action QuestsAction = do
        quests <- query @Quest |> fetch
        render IndexView { .. }

    action NewQuestAction { worldId } = do
        let quest = newRecord
        	|> set #worldId worldId
        world <- fetch worldId        	
        render NewView { .. }

    action ShowQuestAction { questId } = do
        quest <- fetch questId
                 >>= fetchRelated #cells
  	world <- fetch quest.worldId                  
        render ShowView { .. }

    action EditQuestAction { questId } = do
        quest <- fetch questId                  
        render EditView { .. }

    action UpdateQuestAction { questId } = do
        quest <- fetch questId
        quest
            |> buildQuest
            |> ifValid \case
                Left quest -> render EditView { .. }
                Right quest -> do
                    quest <- quest |> updateRecord
                    setSuccessMessage "Quest updated"
                    redirectTo EditQuestAction { questId = quest.id }
	
    action CreateQuestAction = do
        let quest = newRecord @Quest
        quest
            |> buildQuest
            |> ifValid \case
                Left quest -> do
                	world <- fetch quest.worldId 
                	render NewView { .. } 
                Right quest -> do
                    quest <- quest |> createRecord                    
                    setSuccessMessage "Quest created"
                    redirectTo ShowQuestAction { questId = quest.id }

    action DeleteQuestAction { questId } = do
        quest <- fetch questId			  
        deleteRecord quest
        setSuccessMessage "Quest deleted"
        redirectTo QuestsAction

buildQuest quest = quest
	|> set #org True
    |> fill @'["worldId","name"]

