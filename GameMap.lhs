Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module GameMap where

> import Text.JSON 
> import qualified Data.Map as DMap

> import Utils


> data GameMap = GameMap
>     {
>          gmHeight :: Int,
>          gmWidth :: Int,
>          gmTileSetName :: String,
>          gmLocations :: DMap.Map Point GameMapLoc
>     } deriving (Eq, Show)

> data GameMapLoc = GameMapLoc
>     {
>          gmlTileName :: String
>     } deriving (Eq, Show)


> instance JSON GameMap where
>     showJSON gm = makeObj
>         [ ("height", showJSON $ gmHeight gm)
>         , ("width", showJSON $ gmWidth gm) 
>         , ("tileSetName", showJSON $ gmTileSetName gm)
>         , ("locations", showJSON $ gmLocations gm)
>         ]
>
>     readJSON (JSObject obj) = do
>         let objA = fromJSObject obj
>         h <- lookupM "height" objA >>= readJSON
>         w <- lookupM "width" objA >>= readJSON
>         tsn <- lookupM "tileSetName" objA >>= readJSON
>         locs <- lookupM "locations" objA >>= readJSON
>         return $ GameMap h w tsn locs

> instance JSON GameMapLoc where
>     showJSON gml = makeObj
>         [ ("tileName", showJSON $ gmlTileName gml)
>         ]
>
>     readJSON (JSObject obj) = do
>         let objA = fromJSObject obj
>         tn <- lookupM "tileName" objA >>= readJSON
>         return $ GameMapLoc tn