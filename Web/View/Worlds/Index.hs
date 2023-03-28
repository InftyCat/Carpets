module Web.View.Worlds.Index where
import Web.View.Prelude

data IndexView = IndexView { worlds :: [World]  }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>Index<a href={pathTo NewWorldAction} class="btn btn-primary ms-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>World</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach worlds renderWorld}</tbody>
            </table>
            
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Worlds" WorldsAction
                ]

renderWorld :: World -> Html
renderWorld world = [hsx|
    <tr>
        <td>{world.name}</td>
        <td><a href={ShowWorldAction world.id}>Show</a></td>
        <td><a href={EditWorldAction world.id} class="text-muted">Edit</a></td>
        <td><a href={DeleteWorldAction world.id} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
