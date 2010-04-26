Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module Utils where

A module of helper function common to multiple places in the project.

> import Data.Map (Map)
> import qualified Graphics.UI.SDL as SDL


Defines the application name to be used.

> appName = "ExitStrategy"



A monadic lookup function ripped from Magnus's post:
http://therning.org/magnus/archives/719

> lookupM a as = maybe (fail $ "No such element: " ++ a) return (lookup a as)


Helpful type aliases.

> type Point = (Int, Int)
> type TerrainID = String
> type TerrainSurface = (TerrainID, SDL.Surface)
> type TerrainSurfaces = [TerrainSurface]


Gets the ID from the TerrainSurface pair.

> getTerrainSurfaceID :: TerrainSurface -> TerrainID
> getTerrainSurfaceID ts = fst $ ts



This generates a list of map coordinates based off of the
size passed in (columns, rows). 

> mapCoordinates :: (Int , Int) -> [Point]
> mapCoordinates (c , r) = [(x,y) | x <- [1..c]
> 	      	   	          , y <- [1..r]]


Returns a pair representing (width, height) fo the SDL.Surface.

> getSurfaceWH :: SDL.Surface -> (Int, Int)
> getSurfaceWH s = 
>     let w = SDL.surfaceGetWidth s
>         h = SDL.surfaceGetHeight s
>     in 
>         (w , h)
