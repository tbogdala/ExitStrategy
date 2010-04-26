Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module GameMap where

This module defines the data relating to the game map.
JSON is used to save and load the map.
System.Path functions should be used for all file path manipulations.

> import Text.JSON 
> import System.FilePath as FP
> import System.Directory as Dir
> import qualified Data.Map as DMap
> import qualified System.Random as R
> import qualified Control.Monad as CM

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



