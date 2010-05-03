Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module TileSet where

This module houses functions related to TileSet objects.

> import Data.Either
> import Text.JSON 
> import System.FilePath
> import qualified Graphics.UI.SDL as SDL
> import qualified Graphics.UI.SDL.Image as SDLi

> import Utils



> data ResolutionSurfaces = ResolutionSurfaces 
>     {
>         rsiResolutionInfo :: ResolutionInfo,
>         rsiTerrainSurfaces :: [(String, SDL.Surface)],
>         rsiUnitSurfaces :: [(String, SDL.Surface)]
>     }

Define the current version of the TileSet file format.

> currentTileSetFileVersion = 0


Helper function to codify what the current resolution is.

> currentResolution :: [ResolutionSurfaces] -> ResolutionSurfaces
> currentResolution = head 


Defines a TileSet. This is meant to be a way the user can customize
the look of the game with new graphics. 

The name of a tile set is references in a GameMap save file.

For each ResolutionInfo defined, a set of surfaces will be loaded
for the MapTiles listed here. With 3 resolutions and 6 tiles, a total
of 18 surfaces will be loaded.

> data TileSet = TileSet
>     {
>       tsFileVersion :: Int,
>       tsName :: String,
>       tsDefaultTileName :: String,
>       tsResolutions :: [ResolutionInfo],
>       tsTiles :: [MapTile],
>       tsUnits :: [UnitTile]
>     } deriving (Eq, Show)


ResolutionInfo is a way to support multiple graphic resolutions. A set
of MapTiles will be loaded for each ResolutionInfo defined.

riTileWidth and riTileHeight should be set to the dimensions of the
hex tiles for the resolution.

riDirPath should be a list of strings that can be combined into a 
relative path (e.g. ["art", "64x74"] for "art/64x74").

> data ResolutionInfo = ResolutionInfo
>     {
>       riDirPath :: [String],
>       riTileWidth :: Int,
>       riTileHeight :: Int
>     } deriving (Eq, Show)



Each MapTile has a name, which will be used as a TerrainID and a
file name to load. The file name should reside in the ResolutionInfo's
riDirPath.

> data MapTile = MapTile
>     {
>       mtName :: String,
>       mtFileName :: String
>     } deriving (Eq, Show)


Each UnitTile has a name, which is used as an ID, an a file name
to load. The file name should reside in the ResolutionInfo's ridirPath.

> data UnitTile = UnitTile
>     {
>      utName :: String,
>      utFileName :: String
>     } deriving (Eq, Show)


Helper function that loads a TileSet from a file. 
If the TileSet cannot be parsed from the JSON file Left is returned.

> getTileSetFromFile :: String -> IO (Either String TileSet)
> getTileSetFromFile fp = do
>     tryRead `catch` (\_ -> return $ Left $ "Error reading tile set file. (" ++ fp ++ ")")
>   where
>     tryRead = do
>         f <- readFile fp
>         let ts =  decode f :: Result TileSet
>         case ts of
>             Ok set -> return $ Right set
>             Error s -> return $ Left $ "Error parsing tileset (" ++ fp ++  "): " ++ s


Boilerplate code to encode/decode JSON instances of TileSet and related 
data types. See the GameMap.lhs file for further commentary on this.

> instance JSON TileSet where
>    showJSON ts = makeObj
>        [ ("version", showJSON $ tsFileVersion ts)
>        , ("name", showJSON $ tsName ts)
>        , ("defaultTileName", showJSON $ tsDefaultTileName ts)
>        , ("resolutions", showJSON $ tsResolutions ts)
>        , ("tiles", showJSON $ tsTiles ts)
>        , ("units", showJSON $ tsUnits ts)
>        ]
>
>    readJSON (JSObject obj) = do
>        let objA = fromJSObject obj
>        v <- lookupM "version" objA >>= readJSON
>        name <- lookupM "name" objA >>= readJSON
>        defaultTile <- lookupM "defaultTileName" objA >>= readJSON
>        resDirs <- lookupM "resolutions" objA >>= readJSON
>        tiles <- lookupM "tiles" objA >>= readJSON
>        units <- lookupM "units" objA >>= readJSON
>        return $ TileSet v name defaultTile resDirs tiles units

> instance JSON ResolutionInfo where
>    showJSON ri = makeObj
>        [ ("dirPath", showJSON $ riDirPath ri)
>        , ("tileWidth", showJSON $ riTileWidth ri)
>        , ("tileHeight", showJSON $ riTileHeight ri)
>        ]
>
>    readJSON (JSObject obj) = do
>        let objA = fromJSObject obj
>        dp <- lookupM "dirPath" objA >>= readJSON
>        tw <- lookupM "tileWidth" objA >>= readJSON
>        th <- lookupM "tileHeight" objA >>= readJSON
>        return $ ResolutionInfo dp tw th

> instance JSON MapTile where
>    showJSON mt = makeObj
>        [ ("name", showJSON $ mtName mt)
>        , ("fileName", showJSON $ mtFileName mt)
>        ]
>
>    readJSON (JSObject obj) = do
>        let objA = fromJSObject obj
>        n <- lookupM "name" objA >>= readJSON
>        fn <- lookupM "fileName" objA >>= readJSON
>        return $ MapTile n fn

> instance JSON UnitTile where
>     showJSON ut = makeObj
>        [ ("name", showJSON $ utName ut)
>        , ("fileName", showJSON $ utFileName ut)
>        ]
>     readJSON (JSObject obj) = do
>        let objA = fromJSObject obj
>        n <- lookupM "name" objA >>= readJSON
>        fn <- lookupM "fileName" objA >>= readJSON
>        return $ UnitTile n fn



In an IO action, load all of the artwork used in the map. The files are all
referenced inside the TileSet object passed in.

The tileSurfs local function loads all tiles inside a given resolution directory.
The loadFile function will load a given file (mt) in a given directory 
(resInfo.dirPath)


> loadArt :: TileSet -> IO (Either String [ResolutionSurfaces])
> loadArt tileSet = do
>     let mapTiles = (tsTiles tileSet)
>         unitTiles = (tsUnits tileSet)
>     (loadAllSurfs mapTiles unitTiles) `catch` (\_ -> return $ Left $ "Failed to load tiles.")
>   where
>     loadAllSurfs mapTiles unitTiles = do 
>         allSurfs <- mapM (loadResolution mapTiles unitTiles) $ tsResolutions tileSet
>         return $ Right allSurfs
>     loadResolution tiles units resInfo = do
>         usurfs <- mapM (\u -> loadFile resInfo (utName u) (utFileName u)) units
>         tsurfs <- mapM (\t -> loadFile resInfo (mtName t) (mtFileName t)) tiles
>         return $ ResolutionSurfaces resInfo tsurfs usurfs
>     loadFile resInfo dName dFilePath = do
>         let resdir = joinPath (riDirPath resInfo)
>             fp = combine resdir dFilePath
>         s  <- SDLi.load fp
>         s' <- SDL.displayFormatAlpha s
>         SDL.freeSurface s
>         return (dName, s')
