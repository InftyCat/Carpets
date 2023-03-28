module Web.View.Worlds.Show where
import Web.View.Prelude
 
data ShowView = ShowView { world :: Include "quests" World } --

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show World</h1>
        <p>{world}</p>
        <p>Quests:</p>
        <div>{forEach world.quests renderQuest}</div>
        <a href={NewQuestAction (world.id)}> Add Quest </a>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Worlds" WorldsAction
                            , breadcrumbText "Show World"
                            ]
renderQuest quest = if 
	quest.org then [hsx|<a href={ShowQuestAction quest.id}>{quest.name}</a>|]                             
		else [hsx||]
