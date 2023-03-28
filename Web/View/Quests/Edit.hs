module Web.View.Quests.Edit where
import Web.View.Prelude

data EditView = EditView { quest :: Quest }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit Quest</h1>
        {renderForm quest}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Quests" QuestsAction
                , breadcrumbText "Edit Quest"
                ]

renderForm :: Quest -> Html
renderForm quest = formFor quest [hsx|
    
    {submitButton}

|]
