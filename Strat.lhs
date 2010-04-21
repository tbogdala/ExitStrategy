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


Define constants used for this demo related to art and video.

> windowWidth = 640
> windowHeight = 480
> mapRows = 30
> mapColumns = 30
> gameFontFile = "art/fonts/VeraMono.ttf"
> defaultTileSetFP = "art/Default.tiles"
> consoleWidth = 640
> consoleHeight = 200


Generates a variable length list of random TerrainType values.

> getRandomTerrain :: TileSet -> Int -> IO [TerrainType]
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
>      let initialUI = UIState (SDL.Rect 0 0 windowWidth windowHeight) 
>                              [] mainSurf tileSurfs randomMap (mapColumns, mapRows)
>                              uiConsole False defaultKeyHandler
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
>             let oldFirst@(oR, _) = head oldOrder
>             let newFirst@(nR, _) = last oldOrder
>             let rotated = [newFirst] ++ (init oldOrder)
>             if (TS.riTileWidth nR) < (TS.riTileWidth oR)
>                 then eventLoop $ ui { uiTerrainSurfaces = rotated }
>                 else eventLoop ui
>         SDL.MouseButtonDown _ _ SDL.ButtonWheelDown -> do
>             let oldOrder = uiTerrainSurfaces ui
>             let oldFirst@(oR, _) = head oldOrder
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
> defaultKeyHandler ui (SDL.KeyDown (SDL.Keysym SDL.SDLK_q _ _)) = do
>     return Nothing
> defaultKeyHandler ui (SDL.KeyDown (SDL.Keysym SDL.SDLK_w _ _)) = do
>     f <- readFile "art/Default.tiles"
>     let tiles = decode f :: Result TileSet
>     putStrLn $ show tiles
>     return Nothing
> defaultKeyHandler ui (SDL.KeyDown (SDL.Keysym SDL.SDLK_BACKQUOTE _ _)) = do
>     return $ Just $ toggleConsole ui
> defaultKeyHandler ui _ = do return $ Just ui

> consoleKeyHandler :: UIState -> SDL.Event -> IO (DM.Maybe UIState)
> consoleKeyHandler ui (SDL.KeyDown (SDL.Keysym SDL.SDLK_BACKQUOTE _ _)) = do
>     return $ Just $ toggleConsole ui
> consoleKeyHandler ui (SDL.KeyDown (SDL.Keysym SDL.SDLK_RETURN _ _ )) = do
>     return $ Just $ processCurrentLine ui
> consoleKeyHandler ui (SDL.KeyDown (SDL.Keysym SDL.SDLK_BACKSPACE _ _)) = do
>     let c' = removeLastChar (uiConsole ui)
>     return $ Just ui { uiConsole = c' }
> consoleKeyHandler ui (SDL.KeyDown (SDL.Keysym _ _ key)) = do
>     let c' = addCharToConsole (uiConsole ui) key
>     return $ Just ui { uiConsole = c' }



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
>     if (uiConsoleVisible ui)
>         then drawConsole (uiMainSurface ui) (uiConsole ui)
>         else return False

Draw the tile map onto the screen.

> drawMapToScreen :: UIState -> IO ()
> drawMapToScreen ui = do
>     SDL.fillRect (uiMainSurface ui) Nothing (SDL.Pixel 0)
>     mapM_ (drawTile ui) $ mapCoordinates $ uiTerrainMapSize ui




This function converts between a 'game Point' - which is the coordinate in
the game map - to a 'view Point' which is translocated to be relative to the
viewport.

> gamePoint2View :: SDL.Rect -> Point -> Point
> gamePoint2View (SDL.Rect vpx vpy _ _) (gx , gy) = 
>     ((gx - vpx) , (gy - vpy))
		
