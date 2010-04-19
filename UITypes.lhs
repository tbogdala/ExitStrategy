Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module UITypes where

> import qualified Data.Maybe as DM
> import qualified Data.Map as DMap
> import qualified Graphics.UI.SDL as SDL
> import qualified Graphics.UI.SDL.TTF as SDLt


The TerrainType enumeration is used as the key for TerrainSurfaces,
for easy access to the SDL Surface. The TerrainMap has the TerrainType
as a value indexed by a 2d point.

> data TerrainType = TT_Water | TT_Grass | TT_Mountain | TT_Forrest |
>      		     TT_Hills | TT_Desert
>      deriving (Bounded, Eq, Enum, Ord, Show)

Simple definitions to get the max index for the TerrainType enum,
and a list of all enumeration values.

> terrainMaxBound :: Int
> terrainMaxBound = fromEnum (maxBound :: TerrainType)

> terrainTypes :: [TerrainType]
> terrainTypes = enumFrom TT_Water

For now, TerrainMap is linked directly to a TerrainType by
map coordinates.

> type Point = (Int, Int)
> type TerrainSurfaces = [(TerrainType, SDL.Surface)]
> type TerrainMap = DMap.Map Point TerrainType

This generates a list of map coordinates based off of the
size passed in (columns, rows).

> mapCoordinates :: (Int , Int) -> [Point]
> mapCoordinates (c , r) = [(x,y) | x <- [1..r]
> 	   	   	      , y <- [1..c]]


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
> 	  uiTerrainSurfaces :: TerrainSurfaces,
>	  uiTerrainMap :: TerrainMap,
>         uiTerrainMapSize :: (Int , Int),
>         uiConsole :: UIConsole,
>         uiConsoleVisible :: Bool,
>         uiKeyDownHandler :: (UIState -> SDL.Event -> IO (DM.Maybe UIState))
>     }


> data UIConsole = UIConsole 
>     {
>         cViewPort :: SDL.Rect,
>         cSurface :: SDL.Surface,
>         cFont :: SDLt.Font,
>         cTextLog :: [String],
>         cCurrentLine :: String
>     }

