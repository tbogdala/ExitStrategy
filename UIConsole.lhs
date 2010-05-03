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
>     s' <- createConsoleSurface w h
>     return $ UIConsole (SDL.Rect x y w h) s' f [] ""

> createConsoleSurface :: Int -> Int -> IO SDL.Surface
> createConsoleSurface w h = do
>     s <- SDL.createRGBSurface [SDL.SrcAlpha] w h  32 0 0 0 0
>     s' <- SDL.displayFormat s
>     SDL.freeSurface s
>     return s'



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


> updateSurface :: UIConsole -> SDL.Rect -> IO (Maybe UIConsole)
> updateSurface c r = do
>     let prevSurf = cSurface c
>         (pW, pH) = getSurfaceWH prevSurf
>         nW = SDL.rectW r
>         nH = SDL.rectH r
>     if nW == pW || nH == pH
>        then return Nothing
>        else do
>          newSurf <- createConsoleSurface nW nH
>          return $ Just $ c { cSurface = newSurf }




Adds an entire string to the text log of the console.

> addLineToLog :: String -> UIConsole -> UIConsole
> addLineToLog s c = 
>    c { cTextLog = [s] ++ (cTextLog c) }
