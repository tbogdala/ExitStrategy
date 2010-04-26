Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module UITypes where

All of the data types related to user interface is kept here 
(including the TerrainMap for now) to avoid cyclic imports.


> import qualified Data.Maybe as DM
> import qualified Data.Map as DMap
> import qualified Graphics.UI.SDL as SDL
> import qualified Graphics.UI.SDL.TTF as SDLt

> import TileSet
> import Utils
> import GameMap


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
>         uiTileSet :: TileSet,
> 	  uiTerrainSurfaces :: [(ResolutionInfo, TerrainSurfaces)],
>	  uiTerrainMap :: GameMap,
>         uiConsole :: UIConsole,
>         uiConsoleVisible :: Bool,
>         uiCurrentTile :: TerrainID,
>         uiKeyDownHandler :: (UIState -> SDL.Event -> IO (DM.Maybe UIState))
>     }


Helper function to codify what the current resolution is.

> currentResolution :: UIState -> (ResolutionInfo, TerrainSurfaces)
> currentResolution ui = head $ uiTerrainSurfaces ui


Defines the UIConsole data type which has it's own viewport and
SDL.Surface to draw on. Keeps all current input in a separate 
string until processed.

> data UIConsole = UIConsole 
>     {
>         cViewPort :: SDL.Rect,
>         cSurface :: SDL.Surface,
>         cFont :: SDLt.Font,
>         cTextLog :: [String],
>         cCurrentLine :: String
>     }


This helper function creates the SDL surface for tile selection
window. 

Housed in this module until a better home can be found.

> createTileSelectSurface :: Int -> Int -> IO SDL.Surface
> createTileSelectSurface w h = do
>     s <- SDL.createRGBSurface [SDL.SrcAlpha] w h 32 0 0 0 0
>     s' <- SDL.displayFormat s
>     SDL.freeSurface s
>     return s'

