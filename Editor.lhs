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

> loadResources :: IO (Maybe (SDLt.Font, TS.TileSet, [ResolutionSurfaces]))
> loadResources = do
>   fontM <- SDLt.tryOpenFont gameFontFile 16
>   case fontM of
>     Nothing -> putStrLn ("Failed to load font file : " ++ gameFontFile) >> return Nothing
>     Just font -> do
>       tileSetE <- TS.getTileSetFromFile defaultTileSetFile
>       case tileSetE of
>         Left tsErr -> putStrLn ("Failed to load tile set. " ++ tsErr) >> return Nothing
>         Right tileSet -> do
>           resSurfsE <- TS.loadArt tileSet
>           case resSurfsE of
>             Left tssErr -> putStrLn ("Failed to load tiles." ++ tssErr) >> return Nothing
>             Right resSurfs -> return $ Just (font, tileSet, resSurfs)


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
>      freeResSurfs (ResolutionSurfaces _ tsurfs usurfs) = do
>          mapM_ freeSurf tsurfs
>          mapM_ freeSurf usurfs
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
>             let oldResInfo = rsiResolutionInfo $ head oldOrder
>             let newFirst = last oldOrder
>             let rotated = [newFirst] ++ (init oldOrder)
>             if (TS.riTileWidth $ rsiResolutionInfo newFirst) < (TS.riTileWidth oldResInfo)
>                 then eventLoop $ ui { uiTerrainSurfaces = rotated }
>                 else eventLoop ui
>         SDL.MouseButtonDown _ _ SDL.ButtonWheelDown -> do
>             let oldOrder = uiTerrainSurfaces ui
>             let oldResInfo = rsiResolutionInfo $ head oldOrder
>             let newFirst = head $ tail oldOrder
>             let rotated = (tail oldOrder) ++ [(head oldOrder)]
>             if (TS.riTileWidth oldResInfo) < (TS.riTileWidth $ rsiResolutionInfo newFirst)
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
>             resSurfs = currentResolution $ uiTerrainSurfaces ui
>             tileWidth = TS.riTileWidth $ rsiResolutionInfo resSurfs
>             tileHeight = TS.riTileHeight $ rsiResolutionInfo resSurfs
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
>         resSurfs = currentResolution $ uiTerrainSurfaces ui
>         tileWidth = TS.riTileWidth $ rsiResolutionInfo resSurfs
>         tileHeight = TS.riTileHeight $ rsiResolutionInfo resSurfs
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
>     resInfo = currentResolution $ uiTerrainSurfaces ui
>     ts = (rsiTerrainSurfaces resInfo)
>     newID = getNextId currentID 
>     getNextId c = nextLoop c ts ts

>     nextLoop :: TerrainID -> [(String,SDL.Surface)] -> [(String,SDL.Surface)] -> TerrainID
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
>     ts = rsiTerrainSurfaces $ currentResolution $ uiTerrainSurfaces ui
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


Takes the cCurrentLine text string and 'executes' it as a command.
Adds the command string to the log.

> processCurrentLine :: UIState -> IO UIState
> processCurrentLine ui = newUIState
>     where
>        console = uiConsole ui
>        commandWords = words (cCurrentLine console)
>        newLog = [cCurrentLine console] ++ (cTextLog console)
>        newConsole = console { cCurrentLine = "", cTextLog = newLog }
>        newUIState = runCommand commandWords $ ui { uiConsole = newConsole }

These series of commands process the UIConsole 'commands'. Given a [String],
which is just a command string broken up into words, it potentially transforms
a given UIState to a new UIState. 

Some actions may perform IO actions, so the return type is IO UIState.

> runCommand :: [String] -> UIState -> IO UIState

Extends or shrinks the current map. The TileSet's default MapTile
is used if the map dimensions are expanded.

> runCommand (":mapsize" : sW : sH : args) ui = do
>         let w = read sW :: Int
>             h = read sH :: Int
>             oldMap = uiTerrainMap ui
>             newMap = oldMap { gmHeight = h,
>                               gmWidth = w,
>                               gmLocations =  DMap.union cutdownMap defaultMap }
>             defaultMap = foldr makeDefault DMap.empty (mapCoordinates (w,h))
>             cutdownMap = DMap.filterWithKey 
>                 (\(x,y) _ -> if (x<=w)&&(y<=h) then True else False)
>                 (gmLocations oldMap)
>             makeDefault coord m =
>                 DMap.insert coord (GameMapLoc $ tsDefaultTileName $ uiTileSet ui) m
>         return ui { uiTerrainMap = newMap }


Generates a new map entirely randomized with the width and height specified.

> runCommand (":randomize" : sW :sH : args) ui = do
>         let w = read sW :: Int
>         let h = read sH :: Int
>         newMap <- makeRandomMap (uiTileSet ui) w h
>         return $ ui { uiTerrainMap = newMap }


Saves the current map to a file.

> runCommand (":savemap" : mapName : args) ui = do
>     writeMapToFile  mapName $ uiTerrainMap ui
>     let c = addLineToLog "saved map to file." $ uiConsole ui
>     return $ ui { uiConsole = c }


Loads a map from a file.

> runCommand (":loadmap" : mapName : args) ui = do
>     maybeMap <- readMapFromFile mapName 
>     case maybeMap of
>         Just m -> do
>             let ui' = ui { uiTerrainMap = m }
>                 finalUI = addLineToLog "loaded map." $ uiConsole ui'
>             return $ ui { uiConsole = finalUI }
>         Nothing -> do
>             let c = uiConsole ui
>                 c' = addLineToLog "failed to load map file!" c
>             return $ ui { uiConsole = c' }


The default implementation does nothing.
 
> runCommand (cmd : args) ui = do return ui




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
>         vp = uiViewPort ui
>         mainSurf = uiMainSurface ui
>         resSurfs = currentResolution $ uiTerrainSurfaces ui
>     SDL.fillRect (uiMainSurface ui) Nothing (SDL.Pixel 0)
>     drawGameMap gm vp mainSurf resSurfs $ mapCoordinates ((gmWidth gm),(gmHeight gm))

     mapM_ (drawTile gm vp mainSurf resSurfs) $ mapCoordinates ((gmWidth gm),(gmHeight gm))



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
>         ts = rsiTerrainSurfaces $ currentResolution $ uiTerrainSurfaces ui
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



