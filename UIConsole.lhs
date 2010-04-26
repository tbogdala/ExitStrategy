Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module UIConsole where

This file defines the UIConsole which is a Quake-style, drop-down
window in which a user can input commands. Useful for debugging,
or defining lame, difficult-to-use interfaces.


> import Data.Char (isAlphaNum, isSpace, isPunctuation)
> import qualified Data.Maybe as DM
> import qualified Data.Map as DMap
> import qualified Graphics.UI.SDL as SDL
> import qualified Graphics.UI.SDL.TTF as SDLt

> import UITypes
> import Utils
> import TileSet
> import GameMap


Creates the UIConsole object. Also creates a SDL surface used to draw
the console. 

The height of the console is hardcoded to 200 pixels at the moment.

> createUIConsole :: Int ->  Int -> Int -> SDLt.Font -> IO UIConsole
> createUIConsole x y w f = do
>     let h = 200
>     s <- SDL.createRGBSurface [SDL.SrcAlpha] w h  32 0 0 0 0
>     s' <- SDL.displayFormat s
>     SDL.freeSurface s
>     return $ UIConsole (SDL.Rect x y w h) s' f [] ""


Adds the character to the log. Does basic filtering to only allow
alphanumeric, punctuation, and space characters.

> addCharToConsole :: UIConsole -> Char -> UIConsole
> addCharToConsole console c 
>     | isAlphaNum c = updatedConsole
>     | c == ' ' = updatedConsole
>     | isPunctuation c = updatedConsole
>     | otherwise = console
>    where 
>       updatedConsole = console { cCurrentLine = newcmd }
>       newcmd = (cCurrentLine console) ++ [c]


Simple function to remove the last character of the text log.

> removeLastChar :: UIConsole -> UIConsole
> removeLastChar c = c { cCurrentLine = newText }
>     where
>        oldText = (cCurrentLine c)
>        newText = take ((length oldText) - 1) oldText


Takes the cCurrentLine text string and 'executes' it as a command.
Adds the command string to the log.

> processCurrentLine :: UIState -> IO UIState
> processCurrentLine ui = newUIState
>     where
>        console = uiConsole ui
>        commandWords = words (cCurrentLine console)
>        newLog = [cCurrentLine console] ++ (cTextLog console)
>        newConsole = console { cCurrentLine = "", cTextLog = newLog }
>        newUIState = runCommand commandWords $ ui { uiConsole = newConsole }


Simple defaults for the drawing functions.

> consoleTextColor = SDL.Color 255 255 255
> consoleTextLineSpacing = 3 -- pixels
> consoleTextPrompt = "> "


This function draws the console onto its own surface and then
blits it to the main surface. Alpha blending is used to give some
transparency with a hardcoded setting of 160 (0..255 range).

SDL-ttf is used to generate the text surfaces.

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


Adds an entire string to the text log of the console.

> addLineToLog :: String -> UIState -> UIState
> addLineToLog s ui = ui { uiConsole = newConsole}
>     where c = (uiConsole ui)
>           newConsole = c { cTextLog = [s] ++ (cTextLog c) }



These series of commands process the UIConsole 'commands'. Given a [String],
which is just a command string broken up into words, it potentially transforms
a given UIState to a new UIState. 

Some actions may perform IO actions, so the return type is IO UIState.

> runCommand :: [String] -> UIState -> IO UIState
> runCommand (":test" : args) ui = do
>     return $ addLineToLog "Test Successful!" ui


Extends or shrinks the current map. The TileSet's default MapTile
is used if the map dimensions are expanded.

> runCommand (":mapsize" : sW : sH : args) ui = do
>         let w = read sW :: Int
>             h = read sH :: Int
>             oldMap = uiTerrainMap ui
>             newMap = oldMap { gmHeight = h,
>                               gmWidth = w,
>                               gmLocations =  DMap.union cutdownMap defaultMap }
>             defaultMap = foldr makeDefault DMap.empty (mapCoordinates (w,h))
>             cutdownMap = DMap.filterWithKey 
>                 (\(x,y) _ -> if (x<=w)&&(y<=h) then True else False)
>                 (gmLocations oldMap)
>             makeDefault coord m =
>                 DMap.insert coord (GameMapLoc $ tsDefaultTileName $ uiTileSet ui) m
>         return ui { uiTerrainMap = newMap }


Generates a new map entirely randomized with the width and height specified.

> runCommand (":randomize" : sW :sH : args) ui = do
>         let w = read sW :: Int
>         let h = read sH :: Int
>         newMap <- makeRandomMap (uiTileSet ui) w h
>         return $ ui { uiTerrainMap = newMap }


Saves the current map to a file.

> runCommand (":savemap" : mapName : args) ui = do
>     writeMapToFile  mapName $ uiTerrainMap ui
>     return $ addLineToLog "saved map to file." ui


Loads a map from a file.

> runCommand (":loadmap" : mapName : args) ui = do
>     maybeMap <- readMapFromFile mapName 
>     case maybeMap of
>         Just m -> return $ addLineToLog "loaded map." $ ui { uiTerrainMap = m }
>         Nothing -> return $ addLineToLog "failed to load map file!" ui


The default implementation does nothing.
 
> runCommand (cmd : args) ui = do return ui
