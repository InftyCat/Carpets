module Web.View.Cells.New where
import Web.View.Prelude

data NewView = NewView { quest :: Quest , cell :: Cell }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New Cell for { quest.name } </h1>
        {renderForm cell}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Cells" CellsAction
                , breadcrumbText "New Cell"
                ]

renderForm :: Cell -> Html
renderForm cell = formFor cell [hsx|
    {(hiddenField #questId)}
    {(textField #pos)}
    {(textField #direction)}
    {(textField #assumption)}
    {(textField #info)}
    {(textField #definArr)}
    {submitButton}

|]
