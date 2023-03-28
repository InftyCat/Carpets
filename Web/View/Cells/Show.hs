module Web.View.Cells.Show where
import Web.View.Prelude

data ShowView = ShowView { cell :: Cell }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show Cell</h1>
        <p>{cell}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Cells" CellsAction
                            , breadcrumbText "Show Cell"
                            ]