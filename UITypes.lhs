Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module UITypes where

> import qualified Data.Maybe as DM
> import qualified Data.Map as DMap
> import qualified Graphics.UI.SDL as SDL
> import qualified Graphics.UI.SDL.TTF as SDLt

> import TileSet

> type TerrainType = String
> type Point = (Int, Int)
> type TerrainSurfaces = [(String, SDL.Surface)]
> type TerrainMap = DMap.Map Point String

This generates a list of map coordinates based off of the
size passed in (columns, rows).

> mapCoordinates :: (Int , Int) -> [Point]
> mapCoordinates (c , r) = [(x,y) | x <- [1..c]
> 	      	   	          , y <- [1..r]]


All of the state related to user interface is kept here
(including the TerrainMap for now).

uiMouseButtonsDown is tracked due to the SDL.getMouseState
API call not behaving correctly.

uiTerrainSurfaces must be released when exiting game.

> data UIState = UIState 
>     { 
>         uiViewPort :: SDL.Rect,
>	  uiMouseButtonsDown :: [SDL.MouseButton],
>	  uiMainSurface :: SDL.Surface,
> 	  uiTerrainSurfaces :: [(ResolutionInfo, TerrainSurfaces)],
>	  uiTerrainMap :: TerrainMap,
>         uiTerrainMapSize :: (Int , Int),
>         uiConsole :: UIConsole,
>         uiConsoleVisible :: Bool,
>         uiKeyDownHandler :: (UIState -> SDL.Event -> IO (DM.Maybe UIState))
>     }


> currentResolution :: UIState -> (ResolutionInfo, TerrainSurfaces)
> currentResolution ui = head $ uiTerrainSurfaces ui


> data UIConsole = UIConsole 
>     {
>         cViewPort :: SDL.Rect,
>         cSurface :: SDL.Surface,
>         cFont :: SDLt.Font,
>         cTextLog :: [String],
>         cCurrentLine :: String
>     }

