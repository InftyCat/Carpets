module Web.View.Sessions.Index where
import Web.View.Prelude

data IndexView = IndexView { sessions :: [Session]  }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Session</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach sessions renderSession}</tbody>
            </table>
            
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Sessions" SessionsAction
                ]

renderSession :: Session -> Html
renderSession session = [hsx|
    <tr>
        <td>{session.createdAt |> timeAgo } {session.questId }</td>
       
        <td><a href={PlaySessionAction session.id} class="text-muted">Play</a></td>
        <td><a href={DeleteSessionAction session.id} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
-- <td><a href={ShowSessionAction session.id}>Show</a></td>
