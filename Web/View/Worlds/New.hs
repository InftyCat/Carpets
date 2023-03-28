module Web.View.Worlds.New where
import Web.View.Prelude

data NewView = NewView {  world :: World }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New World</h1>
        {renderForm world}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Worlds" WorldsAction
                , breadcrumbText "New World"
                ]

renderForm :: World -> Html
renderForm world = formFor world [hsx|
     {(textField #name)}
    {(textField #columns) }
    {(textField #nodenumber)}
    {(textField #allowednodes)}
    {submitButton}

|]
