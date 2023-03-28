module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

-- Controller Imports
import Web.Controller.Sessions
import Web.Controller.Cells
import Web.Controller.Worlds
import Web.Controller.Quests
import Web.Controller.Static

instance FrontController WebApplication where
    controllers = 
        [ startPage WelcomeAction
        -- Generator Marker
        , parseRoute @SessionsController
        , parseRoute @CellsController
        , parseRoute @WorldsController
        , parseRoute @QuestsController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
