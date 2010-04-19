Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module UIConsole where

> import qualified Graphics.UI.SDL as SDL
> import qualified Graphics.UI.SDL.TTF as SDLt

> import UI


> data UIConsole = UIConsole 
>     {
>         cViewPort :: SDL.Rect,
>         cSurface :: SDL.Surface,
>         cFont :: SDLt.Font,
>         cText :: String
>     }

> createUIConsole :: Int -> Int -> Int -> Int -> SDLt.Font -> IO UIConsole
> createUIConsole x y w h f = do
>     s <- SDL.createRGBSurface [SDL.SrcAlpha] w h 32 0 0 0 0
>     s' <- SDL.displayFormat s
>     return $ UIConsole (SDL.Rect x y w h) s' f ""

> addTextToConsole :: UIConsole -> String -> UIConsole
> addTextToConsole console c = console { cText = newstr }
>     where newstr = (cText console) ++ c



> consoleTextColor = SDL.Color 255 255 255

> drawConsole :: SDL.Surface -> UIConsole -> IO Bool
> drawConsole mainSurface (UIConsole vp surface font text) = do
>     SDL.setAlpha surface [SDL.SrcAlpha] 160
>     SDL.fillRect surface Nothing (SDL.Pixel 0)
>
>     blitText text
>
>     SDL.blitSurface surface Nothing mainSurface $ Just vp
>   where
>     blitText t = do
>         if null text
>           then return Nothing
>           else do
>             textSurf <- SDLt.renderTextSolid font t consoleTextColor 
>             SDL.blitSurface textSurf Nothing surface Nothing
>             SDL.freeSurface textSurf
>             return $ Just t


