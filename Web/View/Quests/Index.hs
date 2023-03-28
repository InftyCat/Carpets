module Web.View.Quests.Index where
import Web.View.Prelude

data IndexView = IndexView { quests :: [Quest]  }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

       
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Quest</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach quests renderQuest}</tbody>
            </table>
            
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Quests" QuestsAction
                ]

renderQuest :: Quest -> Html
renderQuest quest = [hsx|
    <tr>
        <td>{quest.name}</td>
        <td><a href={ShowQuestAction quest.id}>Show</a></td>
        <td><a href={EditQuestAction quest.id} class="text-muted">Edit</a></td>
        <td><a href={DeleteQuestAction quest.id} class="js-delete text-muted">Delete</a></td>
        <!-- #<td><a href={PlayQuestAction quest.id}> Play </a></td> -->
    </tr>
|]
