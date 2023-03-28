module Web.Controller.Worlds where

import Web.Controller.Prelude
import Web.View.Worlds.Index
import Web.View.Worlds.New
import Web.View.Worlds.Edit
import Web.View.Worlds.Show
import Application.Logic.CarpetCentral4 hiding ((|>))
import Application.Logic.Carpets4 hiding (World)
import Application.Logic.Graph4
import Application.Logic.Worlds
instance Controller WorldsController where
    action WorldsAction = do
        worlds <- query @World |> fetch
        render IndexView { .. }

    action NewWorldAction = do
        let world = newRecord
        render NewView { .. }

    action ShowWorldAction { worldId } = do
        world <- fetch worldId
       	          >>= fetchRelated #quests
        render ShowView { .. }

    action EditWorldAction { worldId } = do
        world <- fetch worldId
        render EditView { .. }

    action UpdateWorldAction { worldId } = do
        world <- fetch worldId
        world
            |> buildWorld
            |> ifValid \case
                Left world -> render EditView { .. }
                Right world -> do
                    world <- world |> updateRecord
                    setSuccessMessage "World updated"
                    redirectTo EditWorldAction { .. }

    action CreateWorldAction = do
        let world = newRecord @World
        world
            |> buildWorld
            |> ifValid \case
                Left world -> render NewView { .. } 
                Right world -> do
                    world <- world |> createRecord
                    setSuccessMessage "World created"
                    redirectTo WorldsAction

    action DeleteWorldAction { worldId } = do
        world <- fetch worldId
        deleteRecord world
        setSuccessMessage "World deleted"
        redirectTo WorldsAction

buildWorld world = world
    |> fill @'["name","columns","nodenumber","allowednodes"]
buildWorldInfo :: World -> WorldInfo
buildWorldInfo w = ((infos,(w.columns,w.nodenumber)) , ([],w.allowednodes)) where
	infos :: [Info]
	infos = createPairs [Verti,Hori,Diag]
