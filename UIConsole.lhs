Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module UIConsole where

> import Data.Char (isAlphaNum, isSpace, isPunctuation)
> import qualified Data.Maybe as DM
> import qualified Data.Map as DMap
> import qualified Graphics.UI.SDL as SDL
> import qualified Graphics.UI.SDL.TTF as SDLt

> import UITypes
> import Utils
> import TileSet

> createUIConsole :: Int -> Int -> Int -> Int -> SDLt.Font -> IO UIConsole
> createUIConsole x y w h f = do
>     s <- SDL.createRGBSurface [SDL.SrcAlpha] w h 32 0 0 0 0
>     s' <- SDL.displayFormat s
>     SDL.freeSurface s
>     return $ UIConsole (SDL.Rect x y w h) s' f [] ""

> addCharToConsole :: UIConsole -> Char -> UIConsole
> addCharToConsole console c 
>     | isAlphaNum c = updatedConsole
>     | c == ' ' = updatedConsole
>     | isPunctuation c = updatedConsole
>     | otherwise = console
>    where 
>       updatedConsole = console { cCurrentLine = newcmd }
>       newcmd = (cCurrentLine console) ++ [c]

> removeLastChar :: UIConsole -> UIConsole
> removeLastChar c = c { cCurrentLine = newText }
>     where
>        oldText = (cCurrentLine c)
>        newText = take ((length oldText) - 1) oldText

> processCurrentLine :: UIState -> UIState
> processCurrentLine ui = newUIState
>     where
>        console = uiConsole ui
>        commandWords = words (cCurrentLine console)
>        newLog = [cCurrentLine console] ++ (cTextLog console)
>        newConsole = console { cCurrentLine = "", cTextLog = newLog }
>        newUIState = runCommand commandWords $ ui { uiConsole = newConsole }


> consoleTextColor = SDL.Color 255 255 255
> consoleTextLineSpacing = 3
> consoleTextPrompt = "> "

> drawConsole :: SDL.Surface -> UIConsole -> IO Bool
> drawConsole mainSurface (UIConsole vp surface font textLog currentLine) = do
>     SDL.setAlpha surface [SDL.SrcAlpha] 160
>     SDL.fillRect surface Nothing (SDL.Pixel 0)
>
>     fh <- SDLt.fontHeight font
>     let lineHeight = fh + consoleTextLineSpacing
>
>     let initialY = (SDL.rectH vp) - lineHeight - consoleTextLineSpacing
>     blitText (consoleTextPrompt ++ currentLine) initialY
>     loopLog lineHeight (initialY - lineHeight) textLog
>
>     SDL.blitSurface surface Nothing mainSurface $ Just vp
>   where
>     loopLog _ _ []  = return ()
>     loopLog spacing y (l:ls) = do
>         if (y + spacing < 0)
>             then return ()
>             else do
>                 blitText l y 
>                 loopLog spacing (y - spacing) ls
>
>     blitText t y = do
>         if null t
>           then return Nothing
>           else do
>             textSurf <- SDLt.renderTextSolid font t consoleTextColor 
>             SDL.blitSurface textSurf Nothing surface $ Just $ SDL.Rect 0 y 0 0
>             SDL.freeSurface textSurf
>             return $ Just t



> runCommand :: [String] -> UIState -> UIState
> runCommand (":test" : args) ui = ui { uiConsole = newConsole }
>     where c = (uiConsole ui)
>           newConsole = c { cTextLog = ["Test success!"] ++ (cTextLog c) }
> runCommand (":mapsize" : sW : sH : args) ui = 
>         ui { uiTerrainMap = newMap, uiTerrainMapSize = (w,h) }
>     where
>         w = read sW :: Int
>         h = read sH :: Int
>         cutdownMap = DMap.filterWithKey 
>             (\(x,y) _ -> if (x<=w)&&(y<=h) then True else False)
>             (uiTerrainMap ui)
>         newMap = DMap.union cutdownMap defaultMap
>         defaultMap = foldr makeDefault DMap.empty (mapCoordinates (w,h))
>         makeDefault coord m =
>               DMap.insert coord (tsDefaultTileName $ uiTileSet ui) m

The default implementation does nothing.
 
> runCommand (cmd : args) ui = ui
