module Web.View.Sessions.Play where
import Web.View.Prelude
import Web.View.Quests.Show
import Application.Logic.CarpetCentral4 hiding (Quest)
import Application.Logic.BasicFunctions
import Application.Logic.CarpetTrees3
import Data.List.Split (wordsBy)
import Text.Read
import Data.Text (unpack, pack)
data PlayView = PlayView { world :: World , quest :: Include "cells" Quest , session :: Session }
 
instance View PlayView where
    html PlayView { .. } = let 			
			q = questToLogicQuest world quest.cells
			ptree =  start >>= commandsToAction (session.commands) 
		in  [hsx|
        {breadcrumb}
        <h1>Play Session { quest.name }</h1>
        <span style="white-space: pre">  { q ||> ptree }  </span>		
       
        {renderForm (isJust (q ||> ptree >>~ fin)) session}      
         <form action={ ShowQuestAction quest.id} method="post">
    		<button class="btn-link" >Finish!
    		</button>
		</form>  
    |]
        where			
            breadcrumb = renderBreadcrumb
							[ breadcrumbLink "Sessions" SessionsAction
							, breadcrumbText "Play Session"
							]
renderForm :: Bool -> Session -> Html
renderForm b session = formFor session [hsx|
    {(hiddenField #questId)}
    {(textField #commands) {autofocus = True, disabled = b}}
    {submitButton {label = if b then "Finish" else "Move"}}  

|]
type PPMP = PTreeVC -> MQuest ~-~> PTreeVC
commandsToAction :: [Text] -> PPMP
commandsToAction = foldl (\x y -> x >=> func y) pure
				   . map (wordsBy (==' ') . unpack) where
	func :: [String] -> PPMP
	func (cmd:arr) = translateCommands cmd (map (\a -> read a :: Int) arr)
translateCommands :: String -> [Int] -> PPMP
translateCommands "arg" = arg
translateCommands "spl" = \(x:y:arr) -> spl (x,y) arr >=> tOT
translateCommands "out" = out
{--
 <span style="white-space: pre">  { isJust $ (q ||> ptree >>~ fin) }  </span>		
<form action={CreateSessionAction quest.id } method="post">
        <button type="submit" name="your_name" value="your_value" class="btn-		link">Im finished!
    		</button>
    		</form>
--}

