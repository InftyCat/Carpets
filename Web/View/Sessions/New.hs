module Web.View.Sessions.New where
import Web.View.Prelude

data NewView = NewView { session :: Session }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New Session</h1>
        {renderForm session}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Sessions" SessionsAction
                , breadcrumbText "New Session"
                ]

renderForm :: Session -> Html
renderForm session = formFor session [hsx|
    {(textField #questId)}
    {(textField #commands)}
    {submitButton}

|]