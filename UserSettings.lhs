Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module UserSettings where

This module describes user data that is saved in a configuration file.

> import Text.JSON 
> import Data.Either
> import System.FilePath
> import System.Directory

> import Utils


Define the current version number and default file name for the file.

> currentFileVersion = 0
> defaultUserSettingsFileName = "config"


Last used user settings for various settings.

> data UserSettings = UserSettings
>     {
>         usFileVersion :: Int,
>         usWindowWidth :: Int,
>         usWindowHeight :: Int
>     } deriving (Eq, Show)


> defaultUserSettings :: UserSettings
> defaultUserSettings = UserSettings currentFileVersion 640 480



Boilerplate implementation of the JSON class.

> instance JSON UserSettings where
>     showJSON us = makeObj
>         [ ("version", showJSON $ usFileVersion us)
>         , ("windowWidth", showJSON $ usWindowWidth us)
>         , ("windowHeight", showJSON $ usWindowHeight us)
>         ]
>     readJSON (JSObject obj) = do
>         let objA = fromJSObject obj
>         v <- lookupM "version" objA >>= readJSON
>         w <- lookupM "windowWidth" objA >>= readJSON
>         h <- lookupM "windowHeight" objA >>= readJSON
>         return $ UserSettings v w h 


Writes the user settings to the default file location defined
in defaultUserSettingsFileName.

Returns Left on error or Right on success.
          
> writeUserSettings :: UserSettings -> IO (Either String ())
> writeUserSettings us = do
>     audDir <- getAppUserDataDirectory appName
>     let fp = combine audDir defaultUserSettingsFileName
>     createDirectoryIfMissing True audDir
>     (doWrite fp) `catch` (\_ -> return $ Left "Failed to write user settings file.")
>  where
>     doWrite fp = do writeFile fp $ encode us
>                     return $ Right ()


Reads the user settings from the default file location defined
in defaultUserSettingsFileName.

Returns Left on error or Right on success.

> readUserSettings :: IO (Either String UserSettings)
> readUserSettings = do
>     audDir <-getAppUserDataDirectory appName
>     let fp = combine audDir defaultUserSettingsFileName
>     (tryRead fp) `catch` handleError
>   where
>     handleError e = return $ Left "Failed to read the user settings file."
>     tryRead fp = do
>         f <- readFile fp
>         let json = decode f :: Result UserSettings
>         case json of
>             Ok us -> return $ Right us
>             _ -> return $ Left "Could not parse JSON of user settings file."


