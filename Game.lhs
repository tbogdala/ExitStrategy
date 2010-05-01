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

> import Utils
> import UserSettings as US
> import TileSet as TS
> import UserInterface as UI
> import Server as Server
> import GamePacket as GP

> defaultWindowWidth = 800
> defaultWindowHeight = 600
> gameFontFile = FP.joinPath ["art", "fonts", "VeraMono.ttf"]
> defaultTileSetFile = FP.combine "art" "Default.tiles"
> delayForMaxFPS = quot 1000 60 -- 60 fps target


> getUserSettings :: IO UserSettings
> getUserSettings = do
>     e <- US.readUserSettings
>     case e of 
>         Left err -> putStrLn err >> return US.defaultUserSettings
>         Right us -> return us


This function loads som of the art resources needed and returns them
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
>           tileSurfsE <- TS.loadArt tileSet
>           case tileSurfsE of
>             Left tssErr -> putStrLn ("Failed to load tiles." ++ tssErr) >> return Nothing
>             Right resSurfs -> return $ Just (font, tileSet, resSurfs)


> main :: IO ()
> main = do 
>      CC.forkIO (Server.gameServer GP.defaultPort)
>      SDL.init [SDL.InitEverything]
>      SDLt.init
>      SDL.enableUnicode True 
>      us <- getUserSettings
>      vm <- SDL.trySetVideoMode (US.usWindowWidth us) (usWindowHeight us) 32 
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
>              endState <- MTS.execStateT runGame $
>                          UI.newUIState us font tileSet resSurfs titleScreenLayout False
>              SDL.enableUnicode False
>              SDL.quit
>              wsE <- writeUserSettings $ UI.uisUserSettings endState
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
>         if uisQuitting uis
>             then return ()
>             else (liftIO $ SDL.pollEvent) >>= checkEvent
>     checkEvent e = do
>         case e of
>           SDL.NoEvent -> do
>               drawScreen 
>               liftIO $ SDL.delay delayForMaxFPS
>               e <- liftIO $ SDL.pollEvent
>               checkEvent e
>           SDL.KeyDown ks -> do
>               uis <- MTS.get
>               let cl = uisCurrentLayout uis 
>                   clKH = uilKeyHandler cl
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
>                   cllmbh = uilLMBHandler cl
>               cllmbh (fromIntegral x) (fromIntegral y) SDL.ButtonLeft
>               eventLoop
>           _ -> eventLoop


> drawScreen :: UI.UIStateIO ()
> drawScreen = do
>     uis <- MTS.get
>     mainSurf <- liftIO $ SDL.getVideoSurface
>     liftIO $ SDL.fillRect mainSurf Nothing $ SDL.Pixel 0
>     UI.drawUserInterface $ uisCurrentLayout uis
>     liftIO $ SDL.flip mainSurf
>     return ()
