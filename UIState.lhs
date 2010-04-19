Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module UIState where

> import qualified Data.Maybe as DM
> import qualified Data.Map as DMap
> import qualified Graphics.UI.SDL as SDL
> import qualified Graphics.UI.SDL.TTF as SDLt

> import UI
> import UIConsole


All of the state related to user interface is kept here
(including the TerrainMap for now).

uiMouseButtonsDown is tracked due to the SDL.getMouseState
API call not behaving correctly.

uiTerrainSurfaces must be released when exiting game.

> data UIState = UIState { 
>     	       	 	 uiViewPort :: ViewPort,
>			 uiMouseButtonsDown :: [SDL.MouseButton],
>			 uiMainSurface :: SDL.Surface,
> 			 uiTerrainSurfaces :: TerrainSurfaces,
>			 uiTerrainMap :: TerrainMap,
>                        uiConsole :: UIConsole,
>                        uiConsoleVisible :: Bool,
>                        uiKeyDownHandler :: (UIState -> SDL.Event -> IO (DM.Maybe UIState))
>			 }