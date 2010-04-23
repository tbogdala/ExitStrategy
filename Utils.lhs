Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module Utils where

> import Data.Map (Map)
> import qualified Graphics.UI.SDL as SDL

A monadic lookup function ripped from Magnus's post:
http://therning.org/magnus/archives/719

> lookupM a as = maybe (fail $ "No such element: " ++ a) return (lookup a as)


> type Point = (Int, Int)
> type TerrainID = String
> type TerrainSurface = (TerrainID, SDL.Surface)
> type TerrainSurfaces = [TerrainSurface]
> type TerrainMap = Map Point String

> getTerrainSurfaceID :: TerrainSurface -> TerrainID
> getTerrainSurfaceID ts = fst $ ts

This generates a list of map coordinates based off of the
size passed in (columns, rows).

> mapCoordinates :: (Int , Int) -> [Point]
> mapCoordinates (c , r) = [(x,y) | x <- [1..c]
> 	      	   	          , y <- [1..r]]


> getSurfaceWH :: SDL.Surface -> (Int, Int)
> getSurfaceWH s = 
>     let w = SDL.surfaceGetWidth s
>         h = SDL.surfaceGetHeight s
>     in 
>         (w , h)
