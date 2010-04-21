Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module TileSet where

> import Text.JSON 


> data TileSet = TileSet
>     {
>       tsResolutions :: [ResolutionInfo],
>       tsTiles :: [MapTile]
>     } deriving (Eq, Show)

> data ResolutionInfo = ResolutionInfo
>     {
>       riDirPath :: String,
>       riTileWidth :: Int,
>       riTileHeight :: Int
>     } deriving (Eq, Show)

> data MapTile = MapTile
>     {
>       mtName :: String,
>       mtFileName :: String
>     } deriving (Eq, Show)

A monadic lookup function ripped from Magnus's post:
http://therning.org/magnus/archives/719

> lookupM a as = maybe (fail $ "No such element: " ++ a) return (lookup a as)

Loads a tileset from a file.

> getTileSetFromFile :: String -> IO TileSet
> getTileSetFromFile fp = do
>     f <- readFile fp
>     let ts =  decode f :: Result TileSet
>     case ts of
>         Ok set -> return set
>         Error s -> fail $ "Error parsing tileset (" ++ fp ++  "): " ++ s



> instance JSON TileSet where
>    showJSON ts = makeObj
>        [ ("resolutions", showJSON $ tsResolutions ts)
>        , ("tiles", showJSON $ tsTiles ts)
>        ]
>
>    readJSON (JSObject obj) = do
>        let objA = fromJSObject obj
>        resDirs <- lookupM "resolutions" objA >>= readJSON
>        tiles <- lookupM "tiles" objA >>= readJSON
>        return $ TileSet resDirs tiles

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

