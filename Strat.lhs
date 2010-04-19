Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> import qualified Data.Maybe as DM
> import qualified Data.Map as DMap
> import qualified System.Random as R
> import qualified Control.Monad as CM
> import qualified Graphics.UI.SDL as SDL
> import qualified Graphics.UI.SDL.Image as SDLi
> import qualified Graphics.UI.SDL.TTF as SDLt

> import UITypes
> import UIConsole


List all of the art files that will be used.

NOTE: Must be defined in the same order as the
TerrainType enumeration!

> artFilePaths = [ "art/64x74/water.png",
>  	       	   "art/64x74/grass.png",
>		   "art/64x74/mountain.png",
>		   "art/64x74/forrest.png",
>		   "art/64x74/hills.png",
>		   "art/64x74/desert.png" ]


Define constants used for this demo related to art and video.

> tileWidth = 64
> tileHeight = 74
> windowWidth = 640
> windowHeight = 480
> mapRows = 100
> mapColumns = 100
> gameFontFile = "art/fonts/VeraMono.ttf"
> consoleWidth = 640
> consoleHeight = 200


Generates a variable length list of random TerrainType values.

> getRandomTerrain :: Int -> IO [TerrainType]
> getRandomTerrain l = do
>     randomNumbers <- CM.replicateM l $ R.randomRIO (0,terrainMaxBound)
>     return $ map toEnum randomNumbers


Creates a random 2d map.

> makeRandomMap :: Int -> Int -> IO (TerrainMap)
> makeRandomMap w h = do
>    CM.foldM (\m y -> makeRow w y m) DMap.empty [1..h]
>   where
>      makeRow :: Int -> Int -> TerrainMap -> IO (TerrainMap)
>      makeRow w y tileMap = do
>	   rt <- getRandomTerrain w
>	   let tp = zip [1..w] rt
>	   return $ foldr (\(x,t) m -> DMap.insert (x,y) t m)  tileMap tp


In an IO action, load all of the artwork used in the map.

> loadArt :: [String] -> IO TerrainSurfaces
> loadArt paths = do
>      tileSurfs <- mapM loadFile paths
>      return $ zip terrainTypes tileSurfs
>   where
>      loadFile p = do
>                   s  <- SDLi.load p
>                   s' <- SDL.displayFormatAlpha s
>                   SDL.freeSurface s
>                   return s'

This method draws the TerrainType associated Surface onto another
surface (probably the main screen).

Note that it does not update the destination surface, which will neeed
to be flipped before the effects of this function can be seen.

> drawTile :: UIState -> Point -> IO ()
> drawTile ui (x,y) = do
>      let vp = uiViewPort ui
>          mainSurf = uiMainSurface ui
>          terrainSurfs = uiTerrainSurfaces ui
>          tm = uiTerrainMap ui
>          sr = Just (SDL.Rect 0 0 tileWidth tileHeight)
>          (tX, tY) = gamePoint2View vp $ getHexmapOffset tileWidth tileHeight x y
>	   dr = Just $ SDL.Rect tX tY 0 0
>      	   tt = DM.fromJust $ DMap.lookup (x,y) tm
>      	   terrainSurf = DM.fromJust $ lookup tt terrainSurfs
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
>      tileSurfs <- loadArt artFilePaths
>      randomMap <- makeRandomMap mapColumns mapRows
>
>      font <- SDLt.openFont gameFontFile 16
>      uiConsole <- createUIConsole 0 0 consoleWidth consoleHeight font
>      let initialUI = UIState (SDL.Rect 0 0 windowWidth windowHeight) 
>                              [] mainSurf tileSurfs randomMap (mapColumns, mapRows)
>                              uiConsole False defaultKeyHandler
>      eventLoop initialUI 
>
>      mapM_ freeSurf tileSurfs
>      SDL.enableUnicode False
>      SDL.quit
>      putStrLn "done"
>  where
>      freeSurf (_ , s) = SDL.freeSurface s


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
>                 then eventLoop ui'
>	          else eventLoop ui
>             where	
>                 ui' = ui { uiViewPort = updatedVP }
>                 updatedVP = vp { SDL.rectX = x', SDL.rectY = y' }
>                 vp = uiViewPort ui
>                 x' = (SDL.rectX vp) + fromIntegral xr
>                 y' = (SDL.rectY vp) + fromIntegral yr
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
>     ((gx + vpx) , (gy + vpy))
		
