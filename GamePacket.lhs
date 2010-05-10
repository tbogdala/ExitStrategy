Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module GamePacket where

These are the data structures for the game protocol sent from
player to Server.


> import Data.List (genericDrop)
> import Data.Maybe
> import Network.Socket
> import Text.JSON
> import qualified Data.Map as DM
> import System.IO

> import Utils
> import qualified GameMap as GM

> currentProtocolID = 1
> defaultPortNum = 45954 :: Int
> defaultClientPortNum = 45955 :: Int
> maxPacketSize = 1024 :: Int

> data ClientConInfo = ClientConInfo
>     {
>         cciSocket :: Handle,
>         cciAddress :: SockAddr,
>         cciClientName :: String
>     } 


> data GamePacket = GamePacket
>     {
>         gpCommand :: PacketCommand,
>         gpJSONData :: String
>     } deriving (Eq, Show)

> data PacketCommand = InitGameReq
>                    | InitGameResp
>                    | SetOptions
>                    | OptionsUpdate
>                    | Chat
>                    | ChatUpdate
>                    | GetVisibleMap
>                    | VisibleMapUpdate
>                    | GetVisibleUnits
>                    | VisibleUnitsUpdate
>                    | MoveUnit
>                    | EndTurn
>    deriving (Eq, Show, Enum, Ord)



> data GPBoolResp = GPBoolResp Bool
>                 

> instance JSON GPBoolResp where
>    showJSON (GPBoolResp br) = makeObj
>        [ ("Response", showJSON br) ]
>
>    readJSON (JSObject obj) = do
>        let objA = fromJSObject obj
>        br <- lookupM "Response" objA >>= readJSON
>        return $ GPBoolResp br



> data GameOptions = GameOptions
>    {  
>        goMapWidth :: Int,
>        goMapHeight ::  Int
>    }

> instance JSON GameOptions where
>    showJSON go = makeObj
>        [ ("MapWidth", showJSON $ goMapWidth go) 
>        , ("MapHeight", showJSON $ goMapHeight go)
>        ]
>
>    readJSON (JSObject obj) = do
>        let objA = fromJSObject obj
>        mw <- lookupM "MapWidth" objA >>= readJSON
>        mh <- lookupM "MapHeight" objA >>= readJSON
>        return $ GameOptions mw mh


> createNewPacket :: PacketCommand -> String -> GamePacket
> createNewPacket pc json = GamePacket pc json

> createPacketSetOptions :: GameOptions -> GamePacket
> createPacketSetOptions go =
>     let json = encode go
>     in createNewPacket SetOptions json



> openServerConnection :: HostName -> Int -> String -> IO ClientConInfo
> openServerConnection hostname port playerName = do
>     addrinfos <- getAddrInfo Nothing (Just hostname) (Just $ show port)
>     let serveraddr = head addrinfos
>     sock <- socket (addrFamily serveraddr) Stream defaultProtocol
>     connect sock (addrAddress serveraddr)
>     h <- socketToHandle sock ReadWriteMode
>     hSetBuffering h LineBuffering
>     return $ ClientConInfo h (addrAddress serveraddr) playerName 



> sendPacket :: ClientConInfo -> GamePacket -> IO ()
> sendPacket cci gp = do
>     let gpJSON = encode gp
>     hPutStrLn (cciSocket cci) gpJSON
>     putStrLn $ "SENT: " ++ gpJSON


> readPacket :: ClientConInfo -> IO (Maybe GamePacket)
> readPacket cci = do
>     let h = cciSocket cci
>     incomingMsg <- hReady h
>     if incomingMsg 
>       then readMsg h
>       else return Nothing
>  where
>   readMsg h = do
>     msg <- hGetLine h
>     putStrLn $ "GP: got msg: " ++ msg
>     let egp = getPacketFromMsg msg
>     case egp of
>         Left err -> (putStrLn $ "ERROR (Server/ct): Error getting packet! " 
>                        ++ err) >> return Nothing
>         Right gp -> return $ Just gp



> getPacketFromMsg :: String -> Either String GamePacket
> getPacketFromMsg msg = 
>    let gpr = decode msg :: Result GamePacket
>    in case gpr of
>        Ok gp -> Right gp
>        Error err -> Left $ "Error parsing game packet: " ++ err






JSON encoding of the packets. Will be used to encode things 'down the wire'
once networking is enabled.

> instance JSON PacketCommand where
>    showJSON pc = makeObj [("PCmd", showJSON $ fromEnum  pc)]
>    readJSON (JSObject obj) = do
>        let objA = fromJSObject obj
>        pcInt <- lookupM "PCmd" objA >>= readJSON 
>        return $ toEnum pcInt


Note that the gpSockAddr field isn't serialized at all and is 
initialized to Nothing.

> instance JSON GamePacket where
>    showJSON gp = makeObj
>        [ ("Command", showJSON $ gpCommand gp)
>        , ("JSONData", showJSON $ gpJSONData gp)
>        ]
>
>    readJSON (JSObject obj) = do
>        let objA = fromJSObject obj
>        com <- lookupM "Command" objA >>= readJSON
>        dat <- lookupM "JSONData" objA >>= readJSON
>        return $ GamePacket  com dat
