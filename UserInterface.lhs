Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module UserInterface where

This is the main file for the user interface components.

> import Data.Maybe
> import Data.Either
> import Data.List (sortBy)
> import qualified Control.Monad.Trans.State.Lazy as MTS
> import qualified Data.Map as DM
> import qualified System.FilePath as FP
> import qualified Graphics.UI.SDL as SDL
> import qualified Graphics.UI.SDL.Image as SDLi
> import qualified Graphics.UI.SDL.TTF as SDLt
> import Control.Monad.IO.Class (liftIO)
> import Control.Monad.Trans.Class (lift)

> import Utils
> import qualified TileSet as TS
> import qualified UserSettings as US

This is the data that will be housed in the state.

> data UserInterfaceState = UserInterfaceState
>     {
>         uisUserSettings :: US.UserSettings,
>         uisGameFont :: SDLt.Font,
>         uisTileSet :: TS.TileSet,
>         uisTileSurfaces :: TS.ResolutionTerrainSurfaces,
>         uisLoadedRes :: DM.Map String SDL.Surface 
>     } deriving (Eq, Show)

> data UILayoutPosition = Exact Int Int -- x,y
>                         | CenterScreen Int Int -- xmod ymod
>      deriving (Eq, Show)
>                        

> data UIWidget = UIButton {
>                     uibImageFilePath :: String,
>                     uibPosition :: UILayoutPosition,
>                     uibZOrder :: Int
>                 } deriving (Eq, Show)

> data UILayout = UILayout {
>                     uilWidgets :: [UIWidget]
>                 } deriving (Eq, Show)



> titleScreenLayout :: UILayout
> titleScreenLayout = UILayout 
>     [ (UIButton (FP.joinPath ["art","ui","Background.png"])
>                 (CenterScreen 0 0)
>                 0)
>     , (UIButton (FP.joinPath ["art","ui","MainLogo.png"])
>                 (CenterScreen 0 (-150))
>                 11)
>     , (UIButton (FP.joinPath ["art","ui","SinglePlayer.png"])
>                 (CenterScreen 0 (-50))
>                 12)
>     , (UIButton (FP.joinPath ["art","ui","MultiPlayer.png"])
>                 (CenterScreen 0 0)
>                 13)
>     , (UIButton (FP.joinPath ["art","ui","ExitGame.png"])
>                 (CenterScreen 0 100)
>                 14)
>     ]


> drawUserInterface :: UILayout -> UIStateIO ()
> drawUserInterface uil = do
>    let widgets = getWidgetsByZOrder uil
>    mainSurf <- liftIO $ SDL.getVideoSurface
>    mapM (drawWidget mainSurf) widgets
>    return ()

Returns the list of widgets for a layout ordered by ZOrder.
(lower zorder values first -- ascending)

> getWidgetsByZOrder :: UILayout -> [UIWidget]
> getWidgetsByZOrder uil = sortBy (\a b -> compare (uibZOrder a) (uibZOrder b)) 
>                                 (uilWidgets uil)


> drawWidget :: SDL.Surface -> UIWidget -> UIStateIO (Either String ())
> drawWidget mainSurf uiw = 
>     case uiw of
>       (UIButton fp pos _) -> do
>         btnSurfM <- getUIResource fp
>         case btnSurfM of 
>             Nothing -> return $ Left $ "drawWidget couldn't find " ++ fp
>             Just bs -> do
>                 let dr = getDestRect mainSurf bs pos
>                 liftIO $ SDL.blitSurface bs Nothing mainSurf $ Just dr
>                 return $ Right ()
>   where
>     getDestRect mainSurf widgetSurf pos = 
>       case pos of
>           CenterScreen rx ry -> let (msW,msH) = getSurfaceWH mainSurf
>                                     cx = (quot msW 2) + rx
>                                     cy = (quot msH 2) + ry
>                                     (wdgW,wdgH) = getSurfaceWH widgetSurf
>                                     wx = cx - (quot wdgW 2)
>                                     wy = cy - (quot wdgH 2)
>                                 in SDL.Rect wx wy 0 0
>           Exact x y -> SDL.Rect x y 0 0 

Defines a type using the StateT monad transformer and
the UserInterfaceState struct above to keep track of state.

> type UIStateIO a = MTS.StateT UserInterfaceState IO a


Create a basic instance of UserInterfaceState

> newUIState us f ts rts = UserInterfaceState us f ts rts DM.empty


Retrieves a loaded UI resource surface from the Data.Map, or
loads it into the state and returns the newly loaded surface.

> getUIResource :: FP.FilePath -> UIStateIO (Maybe SDL.Surface)
> getUIResource gari = do
>     uis <- MTS.get
>     let mr = DM.lookup gari $ uisLoadedRes uis
>     case mr of
>         Just r -> return $ Just r
>         Nothing -> do
>             s <- liftIO $ loadFile gari  
>             MTS.put $ uis { uisLoadedRes = DM.insert gari s (uisLoadedRes uis) } 
>             return $ Just s
>   where
>     error _ = return Nothing
>     loadFile fp = do 
>         s  <- SDLi.load fp
>         s' <- SDL.displayFormatAlpha s
>         SDL.freeSurface s
>         return s'
           



