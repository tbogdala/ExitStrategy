Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module Main where

This is the main file for the game executable.

> import Data.Maybe
> import Data.Either
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

> defaultWindowWidth = 800
> defaultWindowHeight = 600
> gameFontFile = FP.joinPath ["art", "fonts", "VeraMono.ttf"]
> defaultTileSetFile = FP.combine "art" "Default.tiles"



> getUserSettings :: IO UserSettings
> getUserSettings = do
>     e <- US.readUserSettings
>     case e of 
>         Left err -> putStrLn err >> return US.defaultUserSettings
>         Right us -> return us

 loadUIResources :: UI.UIStateIO ()
 loadUIResources = do
     let baseDir = FP.combine "art" "ui"
     let testFile = FP.combine "Background.png"
     return $ execStateT (getArtResource testFile) $ UserInterfaceState 

   where
     loadFiles (fn: fns) 


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


> main :: IO ()
> main = do 
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
>            Just (font, tileSet, tileSurfs) -> do
>              MTS.evalStateT runGame $ UI.newUIState us font tileSet tileSurfs
>              SDL.enableUnicode False
>              SDL.quit
>              wsE <- writeUserSettings us
>              case wsE of
>                Left wsErr -> putStrLn wsErr
>                Right _ -> putStrLn "done"

> runGame :: UI.UIStateIO ()
> runGame = do
>     drawScreen
>     eventLoop
>     return ()
>   where
>     eventLoop = (liftIO $ SDL.pollEvent) >>= checkEvent
>     checkEvent e = do
>         case e of
>           SDL.NoEvent -> drawScreen >>  (liftIO $ SDL.waitEventBlocking) >>= checkEvent
>           SDL.KeyDown _ -> return ()
>           SDL.VideoResize x y -> do
>               uis <- MTS.get
>               let us = UI.uisUserSettings uis
>               liftIO $ SDL.setVideoMode x y 32 
>                            [SDL.HWSurface, SDL.DoubleBuf, SDL.Resizable] 
>               MTS.put $ uis { UI.uisUserSettings = us { US.usWindowHeight = y,
>                                                         US.usWindowWidth = x } }
>               liftIO $ putStrLn "Video resize!"
>               eventLoop
>           _ -> eventLoop


> drawScreen :: UI.UIStateIO ()
> drawScreen = do
>     mainSurf <- liftIO $ SDL.getVideoSurface
>     Just bgSurf <- UI.getUIResource "art/ui/Background.png" 
>     Just titleSurf <- UI.getUIResource "art/ui/MainLogo.png"
>     Just singleSurf <- UI.getUIResource "art/ui/SinglePlayer.png"
>     Just multiSurf <- UI.getUIResource "art/ui/MultiPlayer.png"
>     Just exitSurf <- UI.getUIResource "art/ui/ExitGame.png"

>     liftIO $ SDL.fillRect mainSurf Nothing $ SDL.Pixel 0
>     liftIO $ SDL.blitSurface bgSurf Nothing mainSurf Nothing
>     liftIO $ SDL.blitSurface titleSurf Nothing mainSurf $ Just $ SDL.Rect 200 20 0 0
>     liftIO $ SDL.blitSurface singleSurf Nothing mainSurf $ Just $ SDL.Rect 300 150 0 0
>     liftIO $ SDL.blitSurface multiSurf Nothing mainSurf $ Just $ SDL.Rect 300 200 0 0
>     liftIO $ SDL.blitSurface exitSurf Nothing mainSurf $ Just $ SDL.Rect 300 250 0 0
>     liftIO $ SDL.flip mainSurf
>     return ()