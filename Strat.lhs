Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> import qualified Data.Maybe as DM
> import qualified Data.Map as DMap
> import qualified System.Random as R
> import qualified Control.Monad as CM
> import qualified Graphics.UI.SDL as SDL
> import qualified Graphics.UI.SDL.Image as SDLi
> import qualified Graphics.UI.SDL.TTF as SDLt

> import Text.JSON

> import UITypes
> import UIConsole
> import TileSet as TS
> import GameMap
> import Utils


Define constants used for this demo related to art and video.

> windowWidth = 800
> windowHeight = 600
> mapRows = 20
> mapColumns = 20
> gameFontFile = "art/fonts/VeraMono.ttf"
> defaultTileSetFP = "art/Default.tiles"
> consoleWidth = 640
> consoleHeight = 200
> tileSelWindowH = 160
> tileSelWindowW = 160


Generates a variable length list of random TerrainType values.

> getRandomTerrain :: TileSet -> Int -> IO [TerrainID]
> getRandomTerrain ts l = do
>     randomNumbers <- CM.replicateM l $ R.randomRIO (0,(setLength-1))
>     return $ map (\i -> TS.mtName $ (TS.tsTiles ts) !! i) randomNumbers
>   where
>     setLength = length $ TS.tsTiles ts


Creates a random 2d map.

> makeRandomMap :: TileSet -> Int -> Int -> IO (TerrainMap)
> makeRandomMap ts w h = do
>    CM.foldM (\m y -> makeRow w y m) DMap.empty [1..h]
>   where
>      makeRow :: Int -> Int -> TerrainMap -> IO (TerrainMap)
>      makeRow w y tileMap = do
>	   rt <- getRandomTerrain ts w
>	   let tp = zip [1..w] rt
>	   return $ foldr (\(x,t) m -> DMap.insert (x,y) t m)  tileMap tp


In an IO action, load all of the artwork used in the map.

tileSurfs loads all tiles in a given directory (dirFP)
loadFile will load a given file (mt) in a given directory (resInfo.dirPath)

> loadArt :: TileSet -> IO [(TS.ResolutionInfo, TerrainSurfaces)]
> loadArt tileSet = do
>     let mapTiles = (TS.tsTiles tileSet)
>     allSurfs <- mapM (tileSurfs mapTiles) $ TS.tsResolutions tileSet
>     return allSurfs
>   where
>     tileSurfs tiles resInfo = do
>         tsurfs <- mapM (loadFile resInfo) tiles
>         return (resInfo, tsurfs)
>     loadFile resInfo mt = do
>         s  <- SDLi.load $ (TS.riDirPath resInfo) ++ (TS.mtFileName mt)
>         s' <- SDL.displayFormatAlpha s
>         SDL.freeSurface s
>         return ((TS.mtName mt), s')



This method draws the TerrainType associated Surface onto another
surface (probably the main screen).

Note that it does not update the destination surface, which will neeed
to be flipped before the effects of this function can be seen.

> drawTile :: UIState -> Point -> IO ()
> drawTile ui (x,y) = do
>      let vp = uiViewPort ui
>          mainSurf = uiMainSurface ui
>          terrainSurfs = uiTerrainSurfaces ui
>          resSurfs = currentResolution ui
>          tileWidth = TS.riTileWidth $ fst resSurfs
>          tileHeight = TS.riTileHeight $ fst resSurfs
>          tm = uiTerrainMap ui
>          sr = Just (SDL.Rect 0 0 tileWidth tileHeight)
>          (tX, tY) = gamePoint2View vp $ getHexmapOffset tileWidth tileHeight x y
>	   dr = Just $ SDL.Rect tX tY 0 0
>      	   tt = DM.fromJust $ DMap.lookup (x,y) tm
>      	   terrainSurf = DM.fromJust $ lookup tt $ snd resSurfs
>      if tileInViewPort vp tileWidth tileHeight (tX,tY)
>          then do
>	   	 SDL.blitSurface terrainSurf sr mainSurf dr
>		 return ()
>          else 
>      	   	return ()

Takes a viewport and the height and width of the tiles.
The Point passed in should be view-corrected coordinates.
Then it checks for collion between te view rectangle of
  (0, 0) - (vpW, vpH) and the tile rectangle of
  (pX, pY) - (pX', pY').

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
      	      	 

The main worker beast for the program. 

1. Initializes SDL
2. Creates a window and sets its caption
3. Setup the SDL Surfaces for the png file artwork
4. Generate a random map
5. Setup a default UIState and attach all of the data to it.
6. Enter event loop which monitors mouse movement.
7. Pressing a key exits the event loop.
8. Free the SDL Surfaces and quit.

> main :: IO ()
> main = do 
>      SDL.init [SDL.InitEverything]
>      SDLt.init
>      SDL.enableUnicode True 
>      SDL.setVideoMode windowWidth windowHeight 32 
>                       [SDL.HWSurface, SDL.DoubleBuf] 
>      SDL.setCaption "STRAT" "STRAT"
>
>      mainSurf <- SDL.getVideoSurface
>      tileSet <- TS.getTileSetFromFile defaultTileSetFP
>      tileSurfs <- loadArt tileSet
>      randomMap <- makeRandomMap tileSet mapColumns mapRows
>
>      font <- SDLt.openFont gameFontFile 16
>      uiConsole <- createUIConsole 0 0 consoleWidth consoleHeight font
>      tileSelectSurf <- createTileSelectSurface tileSelWindowW tileSelWindowH
>      let initialUI = UIState (SDL.Rect 0 0 windowWidth windowHeight) 
>                              [] mainSurf tileSelectSurf tileSurfs randomMap 
>                              (mapColumns, mapRows)
>                              uiConsole False (tsDefaultTileName tileSet)
>                              defaultKeyHandler
>      eventLoop initialUI 
>
>      mapM_ freeResSurfs tileSurfs
>      SDL.enableUnicode False
>      SDL.quit
>      putStrLn "done"
>  where
>      freeResSurfs (resInfo, tsurfs) = mapM_ freeSurf tsurfs
>      freeSurf (_ , surf) = SDL.freeSurface surf


> eventLoop :: UIState -> IO ()
> eventLoop ui = do
>     e <- SDL.pollEvent
>     case e of
>         SDL.NoEvent -> do
>             redrawScreen ui
>             eventLoop ui
>         SDL.KeyDown _ -> do
>             jui <- (uiKeyDownHandler ui) ui e
>             case jui of
>                 DM.Nothing -> return ()
>                 DM.Just ui' -> eventLoop ui'
>         SDL.MouseMotion _ _ xr yr -> do
>             if elem SDL.ButtonRight $ uiMouseButtonsDown ui
>                 then do
>                      eventLoop ui'
>	          else eventLoop ui
>             where	
>                 resSurfs = currentResolution ui
>                 tileWidth = TS.riTileWidth $ fst resSurfs
>                 tileHeight = TS.riTileHeight $ fst resSurfs
>                 ui' = ui { uiViewPort = updatedVP }
>                 vp = uiViewPort ui
>                 updatedVP = vp { SDL.rectX = x', SDL.rectY = y' }
>                 x' = fix tolXNeg tolXPos $ (SDL.rectX vp) - fromIntegral xr
>                 y' = fix tolYNeg tolYPos $ (SDL.rectY vp) - fromIntegral yr
>                 (tW, tH) = (uiTerrainMapSize ui)
>                 tolXNeg = 0 - (round $ 0.8 * toRational windowWidth)
>                 tolXPos = (tW - 1) * tileWidth 
>                 tolYNeg = 0 - (round $ 0.8 * toRational windowHeight)
>                 tolYPos = (tH - 1) * ( round $ 0.75 *  toRational tileHeight)
>                 fix n p x
>                      | x < n = n
>                      | x > p = p
>                      | otherwise = x
>         SDL.MouseButtonDown _ _ SDL.ButtonWheelUp -> do
>             let oldOrder = uiTerrainSurfaces ui
>             let (oR, _) = head oldOrder
>             let newFirst@(nR, _) = last oldOrder
>             let rotated = [newFirst] ++ (init oldOrder)
>             if (TS.riTileWidth nR) < (TS.riTileWidth oR)
>                 then eventLoop $ ui { uiTerrainSurfaces = rotated }
>                 else eventLoop ui
>         SDL.MouseButtonDown _ _ SDL.ButtonWheelDown -> do
>             let oldOrder = uiTerrainSurfaces ui
>             let (oR, _) = head oldOrder
>             let newFirst@(nR, _) = head $ tail oldOrder
>             let rotated = (tail oldOrder) ++ [(head oldOrder)]
>             if (TS.riTileWidth oR) < (TS.riTileWidth nR)
>                 then eventLoop $ ui { uiTerrainSurfaces = rotated }
>                 else eventLoop ui
>         SDL.MouseButtonDown _ _ b -> do
>             let mbs = uiMouseButtonsDown ui
>             eventLoop $ ui { uiMouseButtonsDown = mbs ++ [b] }
>         SDL.MouseButtonUp _ _ b -> do
>             let mbs = uiMouseButtonsDown ui
>	      let mbs' = filter (\i -> if i == b then False else True) mbs
>             eventLoop $ ui { uiMouseButtonsDown = mbs' }
>         _ -> do eventLoop ui




> defaultKeyHandler :: UIState ->  SDL.Event -> IO (DM.Maybe UIState)
> defaultKeyHandler ui (SDL.KeyDown (SDL.Keysym ks _ key)) = do
>     case ks of
>       SDL.SDLK_q -> return Nothing
>       SDL.SDLK_BACKQUOTE -> return $ Just $ toggleConsole ui
>       SDL.SDLK_RIGHT -> return $ Just $ shiftTileSelectRight ui
>       SDL.SDLK_LEFT -> return $ Just $ shiftTileSelectLeft ui
>       _ -> return $ Just ui
> defaultKeyHandler ui e = do
>    putStrLn $ "defaultKeyHandler wasn't able to handle the event: " ++ (show e)
>    return $ Just ui

> shiftTileSelectRight :: UIState -> UIState
> shiftTileSelectRight ui = ui { uiCurrentTile = newID }
>   where
>     currentID = uiCurrentTile ui
>     (_ , ts) = currentResolution ui
>     newID = getNextId currentID ts
>     getNextId c ts = nextLoop c ts ts

>     nextLoop :: TerrainID -> TerrainSurfaces -> TerrainSurfaces -> TerrainID
>     nextLoop c all [] = getTerrainSurfaceID $ head $ all
>     nextLoop c all (t : ts) = 
>         if c == getTerrainSurfaceID t
>           then 
>             if ts /= []
>              then getTerrainSurfaceID $ head $ ts
>              else getTerrainSurfaceID $ head $ all
>           else nextLoop c all ts

> shiftTileSelectLeft :: UIState -> UIState
> shiftTileSelectLeft ui = ui { uiCurrentTile = newID }
>   where
>     currentID = uiCurrentTile ui
>     (_ , ts) = currentResolution ui
>     newID = getNextId currentID ts
>     getNextId c ts = nextLoop c ts (head ts) (tail ts)

>     nextLoop c all t [] = getTerrainSurfaceID t
>     nextLoop c all t (nt : ts) = 
>         if c == getTerrainSurfaceID nt
>            then getTerrainSurfaceID t
>            else nextLoop c all nt ts 




> consoleKeyHandler :: UIState -> SDL.Event -> IO (DM.Maybe UIState)
> consoleKeyHandler ui (SDL.KeyDown (SDL.Keysym ks _ key)) = do
>     case ks of
>       SDL.SDLK_BACKQUOTE -> return $ Just $ toggleConsole ui
>       SDL.SDLK_RETURN -> return $ Just $ processCurrentLine ui
>       SDL.SDLK_BACKSPACE -> 
>           let c' = removeLastChar (uiConsole ui)
>           in return $ Just ui { uiConsole = c' }
>       _ -> 
>           let c' = addCharToConsole (uiConsole ui) key
>           in return $ Just ui { uiConsole = c' }
> consoleKeyHandler ui e = do
>    putStrLn $ "consoleKeyHandler wasn't able to handle the event: " ++ (show e)
>    return $ Just ui








> toggleConsole :: UIState -> UIState
> toggleConsole ui =  
>     if uiConsoleVisible ui
>      then ui { uiConsoleVisible = False , uiKeyDownHandler = defaultKeyHandler } 
>      else ui { uiConsoleVisible = True , uiKeyDownHandler = consoleKeyHandler }

This redraws the entire screen. This is done in layers.

> redrawScreen ::  UIState -> IO ()
> redrawScreen ui = do
>     drawMapToScreen ui
>     drawUIToScreen ui
>     SDL.flip $ uiMainSurface ui

> drawUIToScreen :: UIState -> IO Bool
> drawUIToScreen ui = do
>     drawSelectedTile ui
>     if (uiConsoleVisible ui)
>         then drawConsole (uiMainSurface ui) (uiConsole ui)
>         else return False

Draw the tile map onto the screen.

> drawMapToScreen :: UIState -> IO ()
> drawMapToScreen ui = do
>     SDL.fillRect (uiMainSurface ui) Nothing (SDL.Pixel 0)
>     mapM_ (drawTile ui) $ mapCoordinates $ uiTerrainMapSize ui

> getSurfaceWH :: SDL.Surface -> (Int, Int)
> getSurfaceWH s = 
>     let w = SDL.surfaceGetWidth s
>         h = SDL.surfaceGetHeight s
>     in 
>         (w , h)

> drawSelectedTile :: UIState -> IO ()
> drawSelectedTile ui = do
>     let mainSurf = uiMainSurface ui
>     let tselSurf = uiSelectedTileSurface ui
>     let (tselW, tselH) = getSurfaceWH tselSurf
>     let (mainW, mainH) = getSurfaceWH mainSurf
>
>     let (_ , ts) = currentResolution ui
>     let Just tileSurf = lookup (uiCurrentTile ui) ts
>     let (tileW, tileH) = getSurfaceWH tileSurf
>     let clipW = tileW + 10
>     let clipH = tileH + 10

>     let pf = SDL.surfaceGetPixelFormat tselSurf
>     white <- SDL.mapRGB pf 255 255 255
>     SDL.fillRect tselSurf Nothing white
>     SDL.fillRect tselSurf (Just $ SDL.Rect 5 5 (clipW-10) (clipH-10)) (SDL.Pixel 0)
>     SDL.blitSurface tileSurf Nothing tselSurf $
>         Just $ SDL.Rect 5 5 0 0
>     SDL.blitSurface tselSurf 
>         (Just $ SDL.Rect 0 0 clipW clipH)
>         (uiMainSurface ui) 
>         (Just $ SDL.Rect (mainW - clipW) (mainH - clipH) 0 0)
>     return ()


This function converts between a 'game Point' - which is the coordinate in
the game map - to a 'view Point' which is translocated to be relative to the
viewport.

> gamePoint2View :: SDL.Rect -> Point -> Point
> gamePoint2View (SDL.Rect vpx vpy _ _) (gx , gy) = 
>     ((gx - vpx) , (gy - vpy))
		
