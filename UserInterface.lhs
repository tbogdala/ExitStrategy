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
> import Control.Monad (filterM)

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

> data UILayoutPosition = Exact Int Int          -- x,y
>                         | CenterScreen Int Int -- xmod ymod
>      deriving (Eq, Show)



> data UIWidget = UIButton {
>                     uibImageFilePath :: String,
>                     uibPosition :: UILayoutPosition,
>                     uibZOrder :: Int,
>                     uibAction :: [UIAction]
>                 } deriving (Eq, Show)

> data UILayout = UILayout {
>                     uilWidgets :: [UIWidget]
>                 } deriving (Eq, Show)

> data UIAction = SwitchGameState GameState
>                | ExitGame
>     deriving (Eq, Show)


All possible main game states.

> data GameState = TitleScreen
>                | MultiplayerTypeSelect
>     deriving (Eq, Show)

> multiplayerTypeSelectLayout :: UILayout
> multiplayerTypeSelectLayout = UILayout
>     [ (UIButton (FP.joinPath ["art","ui","HotSeat.png"])
>                 (CenterScreen 0 (-150))
>                 0 [])
>     ]

> titleScreenLayout :: UILayout
> titleScreenLayout = UILayout 
>     [ (UIButton (FP.joinPath ["art","ui","Background.png"])
>                 (CenterScreen 0 0)
>                 0 [])
>     , (UIButton (FP.joinPath ["art","ui","MainLogo.png"])
>                 (CenterScreen 0 (-150))
>                 11 [])
>     , (UIButton (FP.joinPath ["art","ui","SinglePlayer.png"])
>                 (CenterScreen 0 (-50))
>                 12 [])
>     , (UIButton (FP.joinPath ["art","ui","MultiPlayer.png"])
>                 (CenterScreen 0 0)
>                 13 [SwitchGameState MultiplayerTypeSelect])
>     , (UIButton (FP.joinPath ["art","ui","ExitGame.png"])
>                 (CenterScreen 0 100)
>                 14 [ExitGame])
>     ]


> getWidgetsForClick :: UILayout -> Int -> Int -> UIStateIO ([UIWidget])
> getWidgetsForClick uil x y = do
>     let widgets = reverse $ getWidgetsByZOrder uil
>     mainSurf <- liftIO $ SDL.getVideoSurface
>     hits <- filterM (isCoordInsideWidget mainSurf x y) widgets
>     return hits
>   where
>     isCoordInsideWidget mainSurf x y w = 
>       case w of
>         (UIButton fp pos _ _) -> do
>           btnSurfM <- getUIResource fp
>           case btnSurfM of 
>               Nothing -> return False
>               Just bs -> do
>                 (SDL.Rect wx wy ww wh) <- getPositionRect mainSurf bs pos
>                 if wx <= x && x <= (wx+ww) && wy <= y && y <= (wy+wh)
>                   then return True
>                   else return False


Draws all user interface components listed in the UILayout in
increasing order of Z-Order.

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


Performs the actual blitting onto the surface.

> drawWidget :: SDL.Surface -> UIWidget -> UIStateIO (Either String () )
> drawWidget mainSurf uiw = 
>     case uiw of
>       (UIButton fp pos _ _) -> do
>         btnSurfM <- getUIResource fp
>         case btnSurfM of 
>             Nothing -> return $ Left $ "drawWidget couldn't find " ++ fp
>             Just bs -> do
>                 dr <- getPositionRect mainSurf bs pos
>                 
>                 liftIO $ SDL.blitSurface bs Nothing mainSurf $ Just dr
>                 return $ Right ()


Gets a rectangle for the widget's position on screen. 
All fields of the SDL.Rect are populated.

> getPositionRect :: SDL.Surface -> SDL.Surface -> UILayoutPosition -> UIStateIO (SDL.Rect)
> getPositionRect mainSurf widgetSurf pos = do
>     let (wdgW,wdgH) = getSurfaceWH widgetSurf
>     case pos of
>         CenterScreen rx ry -> let (msW,msH) = getSurfaceWH mainSurf
>                                   cx = (quot msW 2) + rx
>                                   cy = (quot msH 2) + ry
>                                   wx = cx - (quot wdgW 2)
>                                   wy = cy - (quot wdgH 2)
>                               in return $ SDL.Rect wx wy wdgW wdgH
>         Exact x y -> return $ SDL.Rect x y wdgW wdgH 

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
           



