module Web.View.Cells.Index where
import Web.View.Prelude

data IndexView = IndexView { cells :: [Cell]  }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

      
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Cell</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach cells renderCell}</tbody>
            </table>
            
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Cells" CellsAction
                ]

renderCell :: Cell -> Html
renderCell cell = [hsx|
    <tr>
        <td>{cell}</td>
        <td><a href={ShowCellAction cell.id}>Show</a></td>
        <td><a href={EditCellAction cell.id} class="text-muted">Edit</a></td>
        <td><a href={DeleteCellAction cell.id} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
