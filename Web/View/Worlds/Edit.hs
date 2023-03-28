module Web.View.Worlds.Edit where
import Web.View.Prelude

data EditView = EditView { world :: World }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit World</h1>
        {renderForm world}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Worlds" WorldsAction
                , breadcrumbText "Edit World"
                ]

renderForm :: World -> Html
renderForm world = formFor world [hsx|
    
    {submitButton}

|]