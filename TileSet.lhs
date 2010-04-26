Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module TileSet where

This module houses functions related to TileSet objects.

> import Text.JSON 
> import Utils


Defines a TileSet. This is meant to be a way the user can customize
the look of the game with new graphics. 

The name of a tile set is references in a GameMap save file.

For each ResolutionInfo defined, a set of surfaces will be loaded
for the MapTiles listed here. With 3 resolutions and 6 tiles, a total
of 18 surfaces will be loaded.

> data TileSet = TileSet
>     {
>       tsName :: String,
>       tsDefaultTileName :: String,
>       tsResolutions :: [ResolutionInfo],
>       tsTiles :: [MapTile]
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



Helper function that loads a TileSet from a file. If the TileSet
cannot be parsed from the JSON file, fail is called. 

FIXME: Handle the fail case better.

> getTileSetFromFile :: String -> IO TileSet
> getTileSetFromFile fp = do
>     f <- readFile fp
>     let ts =  decode f :: Result TileSet
>     case ts of
>         Ok set -> return set
>         Error s -> fail $ "Error parsing tileset (" ++ fp ++  "): " ++ s


Boilerplate code to encode/decode JSON instances of TileSet and related 
data types. See the GameMap.lhs file for further commentary on this.

> instance JSON TileSet where
>    showJSON ts = makeObj
>        [ ("name", showJSON $ tsName ts)
>        , ("defaultTileName", showJSON $ tsDefaultTileName ts)
>        , ("resolutions", showJSON $ tsResolutions ts)
>        , ("tiles", showJSON $ tsTiles ts)
>        ]
>
>    readJSON (JSObject obj) = do
>        let objA = fromJSObject obj
>        name <- lookupM "name" objA >>= readJSON
>        defaultTile <- lookupM "defaultTileName" objA >>= readJSON
>        resDirs <- lookupM "resolutions" objA >>= readJSON
>        tiles <- lookupM "tiles" objA >>= readJSON
>        return $ TileSet name defaultTile resDirs tiles

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

