module Web.View.Quests.New where
import Web.View.Prelude

data NewView = NewView { world :: World , quest :: Quest } --, worldId :: !(Id World)

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New Quest for {world.name} </h1>
        {renderForm quest}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Quests" QuestsAction
                , breadcrumbText "New Quest"
                ]

renderForm :: Quest -> Html
renderForm quest = formFor quest [hsx|
     {(hiddenField #worldId)}
    {(textField #name)}
    
    {submitButton}

|]
