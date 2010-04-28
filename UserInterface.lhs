Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module UserInterface where

This is the main file for the user interface components.

> import Data.Maybe
> import Data.Either
> import qualified Control.Monad.Trans.State.Lazy as MTS
> import qualified Data.Map as DM
> import qualified System.FilePath as FP
> import qualified Graphics.UI.SDL as SDL
> import qualified Graphics.UI.SDL.Image as SDLi
> import qualified Graphics.UI.SDL.TTF as SDLt
> import Control.Monad.IO.Class (liftIO)
> import Control.Monad.Trans.Class (lift)

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
>     }


Defines a type using the StateT monad transformer and
the UserInterfaceState struct above to keep track of state.

> type UIStateIO a = MTS.StateT UserInterfaceState IO a


Create a basic instance of UserInterfaceState

 newUIState :: SDLt.Font -> TS.TileSet -> TS.ResolutionTerrainSurfaces
            -> UserInterfaceState

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
           



