Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module UITypes where

> import qualified Data.Maybe as DM
> import qualified Data.Map as DMap
> import qualified Graphics.UI.SDL as SDL
> import qualified Graphics.UI.SDL.TTF as SDLt

> import TileSet
> import Utils

All of the state related to user interface is kept here
(including the TerrainMap for now).

uiMouseButtonsDown is tracked due to the SDL.getMouseState
API call not behaving correctly.

uiTerrainSurfaces must be released when exiting game.

The first (ResolutionInfo, TerrainSurface) pair in the array
is considered the current resolution of art to be used.

> data UIState = UIState 
>     { 
>         uiViewPort :: SDL.Rect,
>	  uiMouseButtonsDown :: [SDL.MouseButton],
>	  uiMainSurface :: SDL.Surface,
>         uiSelectedTileSurface :: SDL.Surface,
> 	  uiTerrainSurfaces :: [(ResolutionInfo, TerrainSurfaces)],
>	  uiTerrainMap :: TerrainMap,
>         uiTerrainMapSize :: (Int , Int),
>         uiConsole :: UIConsole,
>         uiConsoleVisible :: Bool,
>         uiCurrentTile :: TerrainID,
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

> createTileSelectSurface :: Int -> Int -> IO SDL.Surface
> createTileSelectSurface w h = do
>     s <- SDL.createRGBSurface [SDL.SrcAlpha] w h 32 0 0 0 0
>     s' <- SDL.displayFormat s
>     SDL.freeSurface s
>     return s'

