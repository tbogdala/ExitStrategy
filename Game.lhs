Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module Main where

This is the main file for the game executable.

> import Data.Maybe
> import Data.Either
> import qualified Control.Concurrent as CC
> import qualified Data.Map as DM
> import qualified System.FilePath as FP
> import qualified Graphics.UI.SDL as SDL
> import qualified Graphics.UI.SDL.Image as SDLi
> import qualified Graphics.UI.SDL.TTF as SDLt
> import qualified Control.Monad.Trans.State.Lazy as MTS
> import Control.Monad.IO.Class (liftIO)
> import Control.Concurrent.Chan
> import Control.Monad

> import Utils
> import qualified UserSettings as US
> import qualified TileSet as TS
> import qualified UserInterface as UI
> import qualified Server as Server
> import qualified GamePacket as GP
> import qualified GameMap as GM
> import qualified UIConsole as UIC


> defaultWindowWidth = 800
> defaultWindowHeight = 600
> gameFontFile = FP.joinPath ["art", "fonts", "VeraMono.ttf"]
> defaultTileSetFile = FP.combine "art" "Default.tiles"
> delayForMaxFPS = quot 1000 60 -- 60 fps target


> getUserSettings :: IO US.UserSettings
> getUserSettings = do
>     e <- US.readUserSettings
>     case e of 
>         Left err -> putStrLn err >> return US.defaultUserSettings
>         Right us -> return us


This function loads som of the art resources needed and returns them
as a giant tuple wrapped in a maybe. If any of these fail to load,
Nothing will be returnede

> loadResources :: IO (Maybe (SDLt.Font, TS.TileSet, [TS.ResolutionSurfaces]))
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
>             Right resSurfs -> return $ Just (font, tileSet, resSurfs)


> main :: IO ()
> main = do 
>      CC.forkIO (Server.gameServer GP.defaultPortNum)
>      SDL.init [SDL.InitEverything]
>      SDLt.init
>      SDL.enableUnicode True 
>      us <- getUserSettings
>      vm <- SDL.trySetVideoMode (US.usWindowWidth us) (US.usWindowHeight us) 32 
>                                [SDL.HWSurface, SDL.DoubleBuf, SDL.Resizable] 
>      if isNothing vm
>        then fail ("Could not set video mode: " ++ (show defaultWindowWidth) ++ 
>                 "x" ++ (show defaultWindowHeight) ++ "x32")
>        else do
>          SDL.setCaption appName appName
>          mainSurf <- SDL.getVideoSurface
>          resM <- loadResources
>          case resM of 
>            Nothing -> putStrLn "Failed to load resources." >> SDL.quit
>            Just (font, tileSet, resSurfs) -> do
>              dummyMap <- GM.makeRandomMap tileSet 30 30
>              console <- UIC.createUIConsole 0 0 200 font
>              endState <- MTS.execStateT runGame $
>                          UI.newUIState us font tileSet resSurfs 
>                                        UI.titleScreenLayout False 
>                                        DM.empty dummyMap
>                                        console

>              SDL.enableUnicode False
>              SDL.quit
>              wsE <- US.writeUserSettings $ UI.uisUserSettings endState
>              case wsE of
>                Left wsErr -> putStrLn wsErr
>                Right _ -> putStrLn "done"


> runGame :: UI.UIStateIO ()
> runGame = do
>     drawScreen
>     eventLoop
>     return ()
>   where
>     eventLoop = do 
>         uis <- MTS.get
>         if UI.uisQuitting uis
>             then return ()
>             else do processNetwork
>                     sdlEvent <- liftIO $ SDL.pollEvent
>                     checkEvent sdlEvent
>                     
>     checkEvent e = do
>         case e of
>           SDL.NoEvent -> do
>               drawScreen 
>               liftIO $ SDL.delay delayForMaxFPS
>               eventLoop
>           SDL.KeyDown ks -> do
>               uis <- MTS.get
>               let cl = UI.uisCurrentLayout uis 
>                   clKH = UI.uilKeyHandler cl
>               clKH ks
>               eventLoop 
>           SDL.VideoResize x y -> do
>               uis <- MTS.get
>               let us = UI.uisUserSettings uis
>               liftIO $ SDL.setVideoMode x y 32 
>                            [SDL.HWSurface, SDL.DoubleBuf, SDL.Resizable] 
>               let us' = us { US.usWindowHeight = y, US.usWindowWidth = x }
>               MTS.put $ uis { UI.uisUserSettings = us'  }
>               eventLoop
>           SDL.MouseButtonUp x y SDL.ButtonLeft -> do
>               uis <- MTS.get
>               let cl = UI.uisCurrentLayout uis
>                   cllmbh = UI.uilLMBHandler cl
>               cllmbh (fromIntegral x) (fromIntegral y) SDL.ButtonLeft
>               eventLoop
>           _ -> eventLoop


> drawScreen :: UI.UIStateIO ()
> drawScreen = do
>     uis <- MTS.get
>     mainSurf <- liftIO $ SDL.getVideoSurface
>     liftIO $ SDL.fillRect mainSurf Nothing $ SDL.Pixel 0
>     UI.drawUserInterface $ UI.uisCurrentLayout uis
>     liftIO $ SDL.flip mainSurf
>     return ()


> processNetwork :: UI.UIStateIO ()
> processNetwork = do
>     uis <- MTS.get
>     let clients = DM.elems (UI.uisPlayerCons uis)
>     mapM_ checkClient clients
>  where
>    checkClient :: GP.ClientConInfo -> UI.UIStateIO ()
>    checkClient cci = do
>      packet <- liftIO $ GP.readPacket cci
>      if(isJust packet) 
>        then checkCallbacks cci $ fromJust packet
>        else return ()



> checkCallbacks :: GP.ClientConInfo -> GP.GamePacket -> UI.UIStateIO ()
> checkCallbacks cci gp = do
>     let cbmap = GP.cciCallbacks cci
>         cbsM = DM.lookup (GP.gpCommand gp) cbmap
>     if isNothing cbsM 
>       then return ()
>       else do
>         let cbs = fromJust cbsM
>         mapM_ (\cb -> liftIO $ cb cci gp) cbs
>         liftIO $ putStrLn "DEBUG: callback found."
>         return ()



     
  
     
