module Web.Controller.Sessions where

import Web.Controller.Prelude
import Web.View.Sessions.Index
import Web.View.Sessions.New
import Web.View.Sessions.Play
import Web.View.Sessions.Show
import Application.Logic.CarpetCentral4 hiding (Quest, (|>) , world)
import Web.View.Quests.Show (questToLogicQuest)
import Data.Text (pack , unpack)
instance Controller SessionsController where
    action SessionsAction = do
        sessions <- query @Session |> fetch
        render IndexView { .. }
    {--action ShowSessionAction { sessionId } = do
        session <- fetch sessionId
        render ShowView { .. }
--}
    action PlaySessionAction { sessionId } = do
        session <- fetch sessionId
        quest <- fetch session.questId
			  >>= fetchRelated #cells
        world <- fetch quest.worldId
       							  
        render PlayView { .. }

    action UpdateSessionAction { sessionId } = do
        session <- fetch sessionId
        session
            |> buildSession
            |> ifValid \case
                Left session -> do
					quest <- fetch session.questId
						  >>= fetchRelated #cells
					world <- fetch quest.worldId
					render PlayView { .. }
                Right session -> do
                    session <- session |> updateRecord  
                    quest <-  fetch session.questId    
						  >>= fetchRelated #cells												
                    world <- fetch quest.worldId
                    if isJust $ ((questToLogicQuest world quest.cells) ||> start >>= commandsToAction (session.commands)  >>~ fin) then
						setSuccessMessage "Session won"
					else 
						setSuccessMessage "Session updated"	                                      
                    redirectTo PlaySessionAction { .. }
	
    action CreateSessionAction { questId } = do
        let session = newRecord @Session |> set #questId questId |> set #commands ([])
        session           
            |> ifValid \case
                Left session -> redirectTo SessionsAction -- render NewView { .. } 
                Right session -> do
                    session <- session |> createRecord         
                   
                    setSuccessMessage "Session created"
                    redirectTo (PlaySessionAction session.id)

    action DeleteSessionAction { sessionId } = do
        session <- fetch sessionId
        deleteRecord session
        setSuccessMessage "Session deleted"
        redirectTo SessionsAction

buildSession session = session
    |> fill @["questId", "commands"]
