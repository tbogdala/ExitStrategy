Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module UI where

> import qualified Data.Map as DMap
> import qualified Graphics.UI.SDL as SDL

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

The ViewPort keeps track of the user's view.

> data ViewPort = ViewPort {
>      	      		   vpX :: Int,
>			   vpY :: Int,
> 			   vpWidth :: Int, 
>			   vpHeight :: Int
>     	      		   }

