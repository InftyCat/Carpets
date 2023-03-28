module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types
data WebApplication = WebApplication deriving (Eq, Show)


data StaticController = WelcomeAction deriving (Eq, Show, Data)

data QuestsController
    = QuestsAction
    | NewQuestAction { worldId :: !(Id World) }
    | ShowQuestAction { questId :: !(Id Quest) }
    | CreateQuestAction
    | EditQuestAction { questId :: !(Id Quest) }
    | UpdateQuestAction { questId :: !(Id Quest) }
    | DeleteQuestAction { questId :: !(Id Quest) }
    -- | CopyQuestAction { questId :: !(Id Quest)  }--, newName :: Text
    | CopyQuestAction { questId :: !(Id Quest) }
    deriving (Eq, Show, Data)

data WorldsController
    = WorldsAction
    | NewWorldAction
    | ShowWorldAction { worldId :: !(Id World) }
    | CreateWorldAction
    | EditWorldAction { worldId :: !(Id World) }
    | UpdateWorldAction { worldId :: !(Id World) }
    | DeleteWorldAction { worldId :: !(Id World) }
    deriving (Eq, Show, Data)

data CellsController
    = CellsAction
    | NewCellAction  { questId :: !(Id Quest)}
    | ShowCellAction { cellId :: !(Id Cell) }
    | CreateCellAction
    | EditCellAction { cellId :: !(Id Cell) }
    | UpdateCellAction { cellId :: !(Id Cell) }
    | DeleteCellAction { cellId :: !(Id Cell) }    
    | CopyMultipleCellsAction { oldQuestId :: !(Id Quest) , newQuestId :: !(Id Quest)}
    deriving (Eq, Show, Data)

data SessionsController
    = SessionsAction
    -- | ShowSessionAction { sessionId :: !(Id Session) }
    | CreateSessionAction { questId :: !(Id Quest) }
    | PlaySessionAction { sessionId :: !(Id Session) }
    | UpdateSessionAction { sessionId :: !(Id Session) }
    | DeleteSessionAction { sessionId :: !(Id Session) }
    deriving (Eq, Show, Data)
