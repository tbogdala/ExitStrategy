Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module GameMap where

This module defines the data relating to the game map.
JSON is used to save and load the map.
System.Path functions should be used for all file path manipulations.

> import Text.JSON 
> import qualified Data.Maybe as DM
> import System.FilePath as FP
> import System.Directory as Dir
> import qualified Data.Map as DMap
> import qualified System.Random as R
> import qualified Control.Monad as CM
> import qualified Graphics.UI.SDL as SDL

> import Utils
> import TileSet as TS


Define the current version number for the file.

> currentGameMapFileVersion = 0

The main game map object.

> data GameMap = GameMap
>     {
>          gmFileVersion :: Int,
>          gmHeight :: Int,
>          gmWidth :: Int,
>          gmTileSetName :: String,
>          gmLocations :: DMap.Map Point GameMapLoc
>     } deriving (Eq, Show)


The data that is specific to a given location.

> data GameMapLoc = GameMapLoc
>     {
>          gmlTileName :: String
>     } deriving (Eq, Show)


Instance delcarations to make JSON encode/decode work.

It appears that using GHC extentions for Data and Typeable only get you
write access and that coding readJSON by hand is the only way to get
the decode functionality.

Nothing fancy is done here; straight boilerplate.

> instance JSON GameMap where
>     showJSON gm = makeObj
>         [ ("version", showJSON $ gmFileVersion gm)
>         , ("height", showJSON $ gmHeight gm)
>         , ("width", showJSON $ gmWidth gm) 
>         , ("tileSetName", showJSON $ gmTileSetName gm)
>         , ("locations", showJSON $ gmLocations gm)
>         ]
>
>     readJSON (JSObject obj) = do
>         let objA = fromJSObject obj
>         v <- lookupM "version" objA >>= readJSON
>         h <- lookupM "height" objA >>= readJSON
>         w <- lookupM "width" objA >>= readJSON
>         tsn <- lookupM "tileSetName" objA >>= readJSON
>         locs <- lookupM "locations" objA >>= readJSON
>         return $ GameMap v h w tsn locs

> instance JSON GameMapLoc where
>     showJSON gml = makeObj
>         [ ("tileName", showJSON $ gmlTileName gml) 
>         ]
>
>     readJSON (JSObject obj) = do
>         let objA = fromJSObject obj
>         tn <- lookupM "tileName" objA >>= readJSON
>         return $ GameMapLoc tn


Generates a variable length (l) list of random MapTile names from
a given TileSet.

> getRandomTerrain :: TileSet -> Int -> IO [TerrainID]
> getRandomTerrain ts l = do
>     randomNumbers <- CM.replicateM l $ R.randomRIO (0,(setLength-1))
>     return $ map (\i -> TS.mtName $ (TS.tsTiles ts) !! i) randomNumbers
>   where
>     setLength = length $ TS.tsTiles ts


Creates a random 2d map based off of the TileSet, width and height
values passed in.

Entire rows are made in each call to getRandomTerrain. foldr is used
to keep track of the Data.Map as GameMapLoc values are inserted.

Because each row is generated in an IO action, foldM is used to
fold them up into an IO result instead of an array of IO actions.

> makeRandomMap :: TileSet -> Int -> Int -> IO (GameMap)
> makeRandomMap ts w h = do
>     map <- CM.foldM (\m y -> makeRow w y m) DMap.empty [1..h]
>     return $ GameMap currentGameMapFileVersion h w (tsName ts) map
>   where
>     makeRow w y tileMap = do
>         rt <- getRandomTerrain ts w
>         let tp = zip [1..w] rt
>         return $ foldr (\(x,t) m -> DMap.insert (x,y) (GameMapLoc t) m)  tileMap tp



Helper function to save the GameMap object to a file. The file
name passed in should not have an extension - one will be automatically
applied.

Files are saved in the Application User Data directory, which, under
UNIX, should be "$HOME/.ExitStrategy/maps". 

If this directory does not exist it will be created.

> writeMapToFile :: String -> GameMap -> IO ()
> writeMapToFile fn gm = do
>     audDir <- Dir.getAppUserDataDirectory appName
>     let dirp = FP.combine audDir "maps"
>     let fp = FP.combine dirp $ FP.replaceExtension fn "map"
>     createDirectoryIfMissing True dirp
>     writeFile fp $ encode gm
>     return ()



Helper function to read the GameMap object from a file. The file
name passed in should not have an extension - one will be automatically
applied.

Files are saved in the Application User Data directory, which, under
UNIX, should be "$HOME/.ExitStrategy/maps". 

If the file does not exist, Nothing is returned.

> readMapFromFile :: String -> IO (Maybe GameMap)
> readMapFromFile fn = do
>     audDir <- Dir.getAppUserDataDirectory appName
>     let dirp = FP.combine audDir  "maps" 
>     let fp = FP.combine dirp $ FP.replaceExtension fn "map"
>     exists <- doesFileExist fp 
>     if exists
>         then do
>             f <- readFile fp
>             let json = decode f :: Result GameMap
>             case json of
>                 Ok gm -> return $ Just gm
>                 _ -> return Nothing
>         else
>             return Nothing


Draws the map tiles, supplied by coords, of the game map onto the 
mainSurf using the resSurf tile surfaces, correctly translated 
view based on vp (viewport).

> drawGameMap :: GameMap -> SDL.Rect -> SDL.Surface -> TS.ResolutionSurfaces -> [Point] -> IO ()
> drawGameMap gm vp mainSurf resSurfs coords = do
>     mapM_ (drawTile gm vp mainSurf resSurfs) coords



This method draws the TerrainType associated Surface onto the main screen's
surface. Note that it does not update the main surface, which will neeed
to be flipped before the effects of this function can be seen.

> drawTile :: GameMap -> SDL.Rect -> SDL.Surface -> TS.ResolutionSurfaces -> Point -> IO ()
> drawTile gm vp mainSurf resSurfs (x,y) = do
>      let tileWidth = TS.riTileWidth $ (rsiResolutionInfo resSurfs)
>          tileHeight = TS.riTileHeight $ (rsiResolutionInfo resSurfs)
>          tm = gmLocations gm
>          sr = Just (SDL.Rect 0 0 tileWidth tileHeight)
>          (tX, tY) = gamePoint2View vp $ getHexmapOffset tileWidth tileHeight x y
>	   dr = Just $ SDL.Rect tX tY 0 0
>      	   gml = DM.fromJust $ DMap.lookup (x,y) tm
>      	   terrainSurf = DM.fromJust $ lookup (gmlTileName gml) $ (rsiTerrainSurfaces resSurfs)
>      if tileInViewPort vp tileWidth tileHeight (tX,tY)
>          then do
>	   	 SDL.blitSurface terrainSurf sr mainSurf dr
>		 return ()
>          else 
>      	   	return ()


Takes a viewport and the height and width of the tiles.
The Point passed in should be view-corrected coordinates.
Then it checks for collion between the view rectangle of
  (0, 0) - (vpW, vpH) and the tile rectangle of
  (pX, pY) - (pX', pY').

Since the point it 'view corrected' we're only concerned with
the height and width of the viewport.

> tileInViewPort :: SDL.Rect -> Int -> Int -> Point -> Bool
> tileInViewPort (SDL.Rect _ _ vpW vpH) tileW tileH (pX , pY) = 
>     let pX' = pX + tileW
>	  pY' = pY + tileH
>     in
>         if ((pX' < 0) || (pX > vpW) || (pY' < 0) || pY > vpH)
>	      then False else True


This does some trickery with numbers to get the tiles to display
in the well known hex grid format. Requires shifting the X value
by half a tile for even rows, and a linear scale of 1/4 a tile
height subtracted from what would otherwise be the y offset.

> getHexmapOffset :: Int -> Int -> Int -> Int -> Point
> getHexmapOffset tileW tileH x y = 
>      (adjX , adjY)
>   where
>      baseAdjX = (tileW * (x-1))
>      baseAdjY = (tileH * (y-1))
>      quarterH = tileH `div` 4
>      halfW = tileW `div` 2
>      adjX = if odd y
>                then baseAdjX + halfW
>		 else baseAdjX
>      adjY = baseAdjY - ((y-1) * quarterH)


calcCoordinate returns a 1-based index as a Point that represents the map tile 
coordinate that the (mx, my) intersects with.

Any reference to even or odd below refer to a 0-based index value, as the result 
is only adjuseted to a 1-based index at the very end of the calculation.

This algoritm accounts for the x-coordinate's tileW/2 offset of even rows. This is handled as
a straight lateral shift of X by tileW/2. Easy.

The y-coordinate is calculated in steps. 1) Find the rounded off y-coord,
2) calculate the remainder of 'my' pixels, 3) Get the slope of the line for the hex
tile, 4) Determine if the y-coord remainder is in the first quarter of the hex tile --
if it is, then figure out whether the remainder puts the point below or above the
hex tile based on the linear line equation.  And this is Haskell, so all of that 
is performed in lazy evaluation.

Another way to look at it is this:

The hex tile is divided up into three sections, shown below (not to scale). The 
size of each section is tileH/4 ... division 1 is considered to overlap with 
the last quarter of the tile.
               _______
         .        1   |
       .   .   _______|   
     |       |  2 & 3 |
     |       | _______|
       .   .         
         v     

For divisons 2 and 3, the offset is calculated easily.

For division 1, the slope of the line is calculated for the left and right
side of the tile. The right slope is chosen, then the y position of the 
line itself is calculated = slope * x - yoffset.  [yoffset is either 
(-tileH / 4) or 0 depending on left or right side, respectively.] Once the 
Y coordinate of the line is known, it is compared to the y offset of the 
point passed into the function.

If the point's y offset is below the line, then we know the map coordinate.
If it's above the line, then the coordinate is adjusted to use the row above. 
This also accounts for the shift between even and odd rows.

Whew.

Some intial inspiration was found here (Author: Paul J. Gyugyi):
http://www-cs-students.stanford.edu/~amitp/Articles/Hexagon1.html

Note: It's important that round is used for scaleH so that it rounds up.
This matches with the way the graphics were generated from svg files.
If the rational number is truncated using quot or floor, there will be
noticable error margins in large maps (50x50 or bigger).

The values should roughly correspond to this:
scaleH = case tileH of
              37 -> 28
              74 -> 56
              148 -> 111


> calcCoordinate :: Int -> Int -> Point -> Point
> calcCoordinate tileW tileH (mx, my) = (flatX + diffX + 1, flatY + diffY + 1)
>     where
>         halfW =  quot tileW  2 
>         scaleH = round $ toRational tileH * 3 / 4
>         flatY = quot my scaleH
>         offsetX = if even flatY 
>                   then halfW 
>                   else 0
>         modifiedMx = mx - offsetX
>         flatX = quot modifiedMx tileW
>         remY = my - (flatY * scaleH)
>         remX = modifiedMx - (flatX * tileW)

>         slopeLeft =  (toRational tileH / 4) / toRational halfW
>         slopeRight = -slopeLeft
>         isLeftHalf = remX < halfW
>         magicY = if isLeftHalf
>                   then abs $ (floor (slopeLeft * realToFrac remX)) - (quot tileH  4)
>                   else abs $ (floor (slopeRight * realToFrac (remX - halfW)))
>         oddOffset = if even flatY then 1 else 0
>         (diffX, diffY) = if (remY > (quot tileH  4)) || (magicY < remY )
>                          then (0,0) 
>                          else if isLeftHalf 
>                               then (-1 + oddOffset, -1) 
>                               else (0 + oddOffset, -1)


Takes the width and height of the graphic tiles, followed by the dimensions
of the terrain map, followed by a 'game point' ... a point in game map
space. The return value is a coordinate into the terrain map.

Wraps calcCoordinate by doing simple bound checking on both the
point passed in and the result returned by calcCoordinate, which
may return 0 in some circumstances.

For any out of bound point, the function returns Nothing.

> getMapCoordinate :: Int -> Int -> (Int,Int) -> Point -> DM.Maybe Point
> getMapCoordinate tileW tileH (maxCol,maxRow) (x,y) = 
>   let quarterH = tileH `div` 4
>       halfW = tileW `div` 2
>       maxX = (maxCol * tileW) + halfW
>       maxY = (maxRow * tileH) - ((maxRow-1) * quarterH)
>       checkPoint p@(px, py) = if px > 0 && px <= maxCol && py > 0 && py <= maxRow 
>                               then Just p
>                               else Nothing
>   in if x < 0 || y < 0
>         then Nothing
>         else if x > maxX || y > maxY
>             then Nothing
>             else checkPoint $ calcCoordinate tileW tileH (x,y)

This function converts between a 'game Point' - which is the coordinate relative to
the game map - to a 'view Point' which is translocated to be relative to the
viewport.

> gamePoint2View :: SDL.Rect -> Point -> Point
> gamePoint2View (SDL.Rect vpx vpy _ _) (gx , gy) = 
>     ((gx - vpx) , (gy - vpy))


		
This function converts between a 'view Point' - which is a coordinate relative to
the viewport - t a 'game point' which is a coordinate relative to the game map.

> viewPoint2Game :: Point -> SDL.Rect -> Point
> viewPoint2Game (x,y) (SDL.Rect vpx vpy _ _) =
>     ((vpx + x) , (vpy + y))