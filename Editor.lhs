Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module Main where

This is the main file for the map editor executable.

> import System.FilePath as FP
> import qualified Data.Maybe as DM
> import qualified Data.Map as DMap
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


Define defaults used for this demo related to art and video.

> defaultWindowWidth = 800
> defaultWindowHeight = 600
> mapRows = 20
> mapColumns = 20

> gameFontFile = FP.joinPath ["art", "fonts", "VeraMono.ttf"]
> defaultTileSetFile = FP.combine "art" "Default.tiles"

Hardcoded constraints for the tile selection window surface. 
This should be bigger than the largest tile reference in the TileSet.

> tileSelWindowH = 200
> tileSelWindowW = 200





This method draws the TerrainType associated Surface onto the main screen's
surface. Note that it does not update the destination surface, which will neeed
to be flipped before the effects of this function can be seen.



> drawTile :: UIState -> Point -> IO ()
> drawTile ui (x,y) = do
>      let vp = uiViewPort ui
>          mainSurf = uiMainSurface ui
>          terrainSurfs = uiTerrainSurfaces ui
>          resSurfs = currentResolution ui
>          tileWidth = TS.riTileWidth $ fst resSurfs
>          tileHeight = TS.riTileHeight $ fst resSurfs
>          tm = gmLocations $ uiTerrainMap ui
>          sr = Just (SDL.Rect 0 0 tileWidth tileHeight)
>          (tX, tY) = gamePoint2View vp $ getHexmapOffset tileWidth tileHeight x y
>	   dr = Just $ SDL.Rect tX tY 0 0
>      	   gml = DM.fromJust $ DMap.lookup (x,y) tm
>      	   terrainSurf = DM.fromJust $ lookup (gmlTileName gml) $ snd resSurfs
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


Sets the video mode based on the width and height passed in.
This function will be called every time the window is resized.

> setVideo :: Int -> Int -> IO (Maybe SDL.Surface)
> setVideo x y = do
>     s <- SDL.trySetVideoMode x y 32 
>              [SDL.HWSurface, SDL.DoubleBuf, SDL.Resizable] 
>     return s

This function loads all of the art resources needed and returns them
as a giant tuple wrapped in a maybe. If any of these fail to load,
Nothing will be returnede

> loadResources :: IO (Maybe (SDLt.Font, TS.TileSet, ResolutionTerrainSurfaces))
> loadResources = do
>   fontM <- SDLt.tryOpenFont gameFontFile 16
>   case fontM of
>     Nothing -> putStrLn ("Failed to load font file : " ++ gameFontFile) >> return Nothing
>     Just font -> do
>       tileSetE <- TS.getTileSetFromFile defaultTileSetFile
>       case tileSetE of
>         Left tsErr -> putStrLn ("Failed to load tile set. " ++ tsErr) >> return Nothing
>         Right tileSet -> do
>           tileSurfsE <- TS.loadArt tileSet
>           case tileSurfsE of
>             Left tssErr -> putStrLn ("Failed to load tiles." ++ tssErr) >> return Nothing
>             Right tileSurfs -> return $ Just (font, tileSet, tileSurfs)


The main worker beast for the program. 

1. Initializes SDL and sub-libraries
2. Creates a window and sets its caption
3. Setup the SDL Surfaces for the png file artwork 
4. Generate a random map
5. Setup a default UIState and attach all of the data to it,
   including a UIConsole and a loaded font
6. Enter event loop.
8. Free the SDL Surfaces and quit.

> main :: IO ()
> main = do 
>      SDL.init [SDL.InitEverything]
>      SDLt.init
>      SDL.enableUnicode True 
>      setVideo defaultWindowWidth defaultWindowHeight
>      SDL.setCaption (appName ++ "- Editor") (appName ++ "-Editor")
>
>      mainSurf <- SDL.getVideoSurface

>      resM <- loadResources
>      case resM of 
>        Nothing -> putStrLn "Failed to load resources." >> SDL.quit
>        Just (font, tileSet, tileSurfs) -> do
>          randomMap <- makeRandomMap tileSet mapColumns mapRows
>
>          font <- SDLt.openFont gameFontFile 16
>          uiConsole <- createUIConsole 0 0 defaultWindowWidth font
>          tileSelectSurf <- createTileSelectSurface tileSelWindowW tileSelWindowH
>          let initialUI = UIState (SDL.Rect 0 0 defaultWindowWidth defaultWindowHeight) 
>                                  [] mainSurf tileSelectSurf tileSet tileSurfs 
>                                  randomMap uiConsole False (tsDefaultTileName tileSet)
>                                  defaultKeyHandler
>          eventLoop initialUI 
>
>          mapM_ freeResSurfs tileSurfs
>          SDL.enableUnicode False
>          SDL.quit
>  where
>      freeResSurfs (resInfo, tsurfs) = mapM_ freeSurf tsurfs
>      freeSurf (_ , surf) = SDL.freeSurface surf


The main event loop. This loop polls events until the quit button
is pressed. 

Note that SDL.pollEvent is used here, which is non-blocking.

> eventLoop :: UIState -> IO ()
> eventLoop ui = do
>     e <- SDL.pollEvent
>     checkEvent ui e


The checkEvent function actually handles a given event with
the UIState data.

Events are polled by calling eventLoop after the event is handled.
When no events are available, the SDL.NoEvent event is received.
At this time, checkEvent will redraw the screen and then block
for an event.

For keyboard input, the uiKeyDownHandler of the UIState is invoked.
If a Nothing value is returned, the function returns, which will
end the eventLoop function.

Mouse button state is tracked in the UIState object.

> checkEvent :: UIState -> SDL.Event -> IO ()
> checkEvent ui e = do
>     case e of
>         SDL.NoEvent -> do
>             redrawScreen ui
>             e <- SDL.waitEventBlocking
>             checkEvent ui e
>         SDL.VideoResize x y -> do
>             let oldVP = uiViewPort ui
>                 oldConsole = uiConsole ui
>             setVideo x y
>             SDL.freeSurface $ cSurface oldConsole
>             newConsole <- createUIConsole 0 0 x (cFont $ oldConsole)
>             eventLoop $ ui { uiViewPort = (SDL.Rect (SDL.rectX oldVP) (SDL.rectY oldVP) x y),
>                              uiConsole = newConsole
>                            }
>         SDL.KeyDown _ -> do
>             jui <- (uiKeyDownHandler ui) ui e
>             case jui of
>                 DM.Nothing -> return ()
>                 DM.Just ui' -> eventLoop ui'
>         SDL.MouseMotion x y xr yr -> do
>             if elem SDL.ButtonRight $ uiMouseButtonsDown ui
>                 then eventLoop $ rightMouseBMHandler ui 
>                                      (fromIntegral xr) 
>                                      (fromIntegral yr)
>                 else if elem SDL.ButtonLeft $ uiMouseButtonsDown ui
>                      then eventLoop $ leftMouseBMHandler
>                                           ui (fromIntegral x) (fromIntegral y)
>                                           (fromIntegral xr) (fromIntegral yr)
>                      else eventLoop ui
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
>         SDL.MouseButtonDown x y b -> do
>             let mbs = uiMouseButtonsDown ui
>             let updUI = ui { uiMouseButtonsDown = mbs ++ [b] }
>             if b == SDL.ButtonLeft
>                then eventLoop $ changeTile (fromIntegral x) (fromIntegral y) updUI
>                else eventLoop updUI
>         SDL.MouseButtonUp _ _ b -> do
>             let mbs = uiMouseButtonsDown ui
>	      let mbs' = filter (/= b) mbs
>             eventLoop $ ui { uiMouseButtonsDown = mbs' }
>         _ -> do eventLoop ui


Changes the map tile under the mouse coordinate passed in to the
selected tile defined in UIState.

If there is not a tile underneath the mouse, nothing is done.

> changeTile :: Int -> Int -> UIState -> UIState
> changeTile x y ui= 
>         let (gx, gy) = viewPoint2Game (x,y) $ uiViewPort ui
>             resSurfs = currentResolution ui
>             tileWidth = TS.riTileWidth $ fst resSurfs
>             tileHeight = TS.riTileHeight $ fst resSurfs
>             gameMap = uiTerrainMap ui
>             c = getMapCoordinate tileWidth tileHeight ((gmWidth gameMap), (gmHeight gameMap)) (gx,gy)
>             newID = uiCurrentTile ui
>             updatedLocations coord = DMap.insert coord (GameMapLoc newID) $ gmLocations gameMap
>         in case c of
>             Just coord -> ui { uiTerrainMap =  (gameMap { gmLocations = (updatedLocations coord) }) }
>             Nothing -> ui


Simply passes through to changeTile.

> leftMouseBMHandler :: UIState ->  Int -> Int -> Int -> Int ->  UIState
> leftMouseBMHandler ui x y xr yr = changeTile x y ui


Handles all panning of the viewpoint. This function also will not allow
the user to scroll away too far; some of the map must remain on screen.

> rightMouseBMHandler :: UIState -> Int -> Int -> UIState
> rightMouseBMHandler ui xr yr =  ui { uiViewPort = updatedVP }
>     where	
>         resSurfs = currentResolution ui
>         tileWidth = TS.riTileWidth $ fst resSurfs
>         tileHeight = TS.riTileHeight $ fst resSurfs
>         windowWidth = SDL.rectW $ uiViewPort ui
>         windowHeight = SDL.rectH $ uiViewPort ui
>         vp = uiViewPort ui
>         updatedVP = vp { SDL.rectX = x', SDL.rectY = y' }
>         x' = fix tolXNeg tolXPos $ (SDL.rectX vp) - fromIntegral xr
>         y' = fix tolYNeg tolYPos $ (SDL.rectY vp) - fromIntegral yr
>         tW = gmWidth $ uiTerrainMap ui
>         tH = gmHeight $ uiTerrainMap ui
>         tolXNeg = 0 - (round $ 0.8 * toRational windowWidth)
>         tolXPos = (tW - 1) * tileWidth 
>         tolYNeg = 0 - (round $ 0.8 * toRational windowHeight)
>         tolYPos = (tH - 1) * ( round $ 0.75 *  toRational tileHeight)
>         fix n p x
>              | x < n = n
>              | x > p = p
>              | otherwise = x



This handles key input for the main editor application. When the console
is active, a different key handler is used.

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


Changes the selected tile to the next one in the list, wrapping back
to the beginning if necessary.

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


Changes the selected tile to the previous one in the list, wrapping to
the end of the list if necessary.

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


This is the keyboard handler used when the UIConsole is being displayed.
Most keys are passed in to the UIConsole as input. RETURN processes 
the command. The ` (backquote) key opens and closes the console.

> consoleKeyHandler :: UIState -> SDL.Event -> IO (DM.Maybe UIState)
> consoleKeyHandler ui (SDL.KeyDown (SDL.Keysym ks _ key)) = do
>     case ks of
>       SDL.SDLK_BACKQUOTE -> return $ Just $ toggleConsole ui
>       SDL.SDLK_RETURN -> do
>           ui' <- processCurrentLine ui
>           return $ Just ui'
>       SDL.SDLK_BACKSPACE -> 
>           let c' = removeLastChar (uiConsole ui)
>           in return $ Just ui { uiConsole = c' }
>       _ -> 
>           let c' = addCharToConsole (uiConsole ui) key
>           in return $ Just ui { uiConsole = c' }
> consoleKeyHandler ui e = do
>    putStrLn $ "consoleKeyHandler wasn't able to handle the event: " ++ (show e)
>    return $ Just ui


Toggles the visibility of the UIConsole

> toggleConsole :: UIState -> UIState
> toggleConsole ui =  
>     if uiConsoleVisible ui
>      then ui { uiConsoleVisible = False , uiKeyDownHandler = defaultKeyHandler } 
>      else ui { uiConsoleVisible = True , uiKeyDownHandler = consoleKeyHandler }



This redraws the entire screen. 
This is done in layers. First the map, then the UI.

> redrawScreen ::  UIState -> IO ()
> redrawScreen ui = do
>     drawMapToScreen ui
>     drawUIToScreen ui
>     SDL.flip $ uiMainSurface ui



Draws the user interface. First the selected tile window is drawn, 
then the UIConsole is drawn if necessary.

> drawUIToScreen :: UIState -> IO Bool
> drawUIToScreen ui = do
>     drawSelectedTile ui
>     if (uiConsoleVisible ui)
>         then drawConsole (uiMainSurface ui) (uiConsole ui)
>         else return False



Draw the tile map onto the screen. All of the hard work is done
in drawTile. mpCoordinates generates a list of (x,y) coordinates
to use instead of using map keys. Map keys are slow.

> drawMapToScreen :: UIState -> IO ()
> drawMapToScreen ui = do
>     let gm = uiTerrainMap ui
>     SDL.fillRect (uiMainSurface ui) Nothing (SDL.Pixel 0)
>     mapM_ (drawTile ui) $ mapCoordinates ((gmWidth gm),(gmHeight gm))


Draws the selected tile window. 
A white pixel fill is done first to give a border to the window. There's 
a hardcoded buffer of 5 pixels given for this

> drawSelectedTile :: UIState -> IO ()
> drawSelectedTile ui = do
>     let mainSurf = uiMainSurface ui
>         tselSurf = uiSelectedTileSurface ui
>         (tselW, tselH) = getSurfaceWH tselSurf
>         (mainW, mainH) = getSurfaceWH mainSurf
>
>         (_ , ts) = currentResolution ui
>         Just tileSurf = lookup (uiCurrentTile ui) ts
>         (tileW, tileH) = getSurfaceWH tileSurf
>         clipW = tileW + 10
>         clipH = tileH + 10
>         pf = SDL.surfaceGetPixelFormat tselSurf

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