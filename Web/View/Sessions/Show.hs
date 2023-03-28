module Web.View.Sessions.Show where
import Web.View.Prelude

data ShowView = ShowView { session :: Session }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show Session</h1>
        <p>{session}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Sessions" SessionsAction
                            , breadcrumbText "Show Session"
                            ]