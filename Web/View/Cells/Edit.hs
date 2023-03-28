module Web.View.Cells.Edit where
import Web.View.Prelude

data EditView = EditView { cell :: Cell }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit Cell</h1>
        {renderForm cell}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Cells" CellsAction
                , breadcrumbText "Edit Cell"
                ]

renderForm :: Cell -> Html
renderForm cell = formFor cell [hsx|
    {(textField #questId)}
    {(textField #pos)}
    {(textField #direction)}
    {(textField #assumption)}
    {submitButton}

|]
