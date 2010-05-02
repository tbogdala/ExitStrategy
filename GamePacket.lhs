Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module GamePacket where

These are the data structures for the game protocol sent from
player to Server.


> import Data.List (genericDrop)
> import Data.Maybe
> import Network.Socket
> import Network.BSD
> import Text.JSON
> import qualified Data.Map as DM

> import Utils

> currentProtocolID = 1
> defaultPortNum = 45954 :: Int
> defaultClientPortNum = 45955 :: Int
> maxPacketSize = 1024 :: Int

> data ClientConInfo = ClientConInfo
>     {
>         cciSocket :: Socket,
>         cciAddress :: SockAddr,
>         cciClientName :: String,
>         cciClientID :: Int,
>         cciLastSeq :: Int,
>         cciNeedAcks :: [GamePacket],
>         cciCallbacks :: DM.Map PacketCommand [PacketCallback]
>     } 

> type PacketCallback = (ClientConInfo -> GamePacket -> IO ())


> data GamePacket = GamePacket
>     {
>         gpSockAddr :: Maybe SockAddr,
>         gpReturnPort :: Int,  
>         gpClientID :: Int,
>         gpSeq :: Int,
>         gpCommand :: PacketCommand,
>         gpJSONData :: String
>     } deriving (Eq, Show)

> data PacketCommand = ACK
>                    | InitGameReq
>                    | InitGameResp
>                    | RequestAsPlayer
>                    | SetOptions
>                    | Chat
>                    | GetVisibleMap
>                    | GetVisibleUnits
>                    | MoveUnit
>                    | EndTurn
>    deriving (Eq, Show, Enum, Ord)


> data GPBoolResp = GPBoolResp Bool
> instance JSON GPBoolResp where
>    showJSON (GPBoolResp br) = makeObj
>        [ ("Response", showJSON br) ]
>
>    readJSON (JSObject obj) = do
>        let objA = fromJSObject obj
>        br <- lookupM "Response" objA >>= readJSON
>        return $ GPBoolResp br



> createNewPacket :: Maybe ClientConInfo -> Int -> PacketCommand -> String ->
>                         (Maybe ClientConInfo, GamePacket)
> createNewPacket cciM returnPort pc json = 
>     let lastSeq = if isNothing cciM then (-1) else cciLastSeq cci
>         newCCI = if isNothing cciM then Nothing else Just $ cci { cciLastSeq = lastSeq + 1 }
>         cci = fromJust cciM
>         clientId = if isNothing cciM then 0 else cciClientID cci
>         gp = GamePacket Nothing returnPort clientId (lastSeq+1) pc json
>     in (newCCI, gp)


 createNewCCI :: ClientConInfo
 createNewCCI = ClientConInfo Nothing Nothing [] (-1) (-1)

Actual networking is disabled for now.

> openServerConnection :: HostName -> Int -> String -> IO ClientConInfo
> openServerConnection hostname port playerName = do
>     addrinfos <- getAddrInfo Nothing (Just hostname) (Just $ show port)
>     let serveraddr = head addrinfos
>     sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
>     connect sock (addrAddress serveraddr)
>     return $ ClientConInfo sock (addrAddress serveraddr) 
>                  playerName 0 0 [] DM.empty



> sendPacket :: ClientConInfo -> GamePacket -> IO ClientConInfo
> sendPacket cci gp = do
>     sendAnonyPacket (cciSocket cci) (cciAddress cci) gp
>     let ackls = cciNeedAcks cci
>     return $ cci { cciNeedAcks = [gp] ++ ackls }


> sendAnonyPacket :: Socket -> SockAddr -> GamePacket -> IO ()
> sendAnonyPacket sock sockaddr gp = do
>     let gpJSON = encode gp
>     sendString gpJSON
>   where
>     sendString :: String -> IO ()
>     sendString [] = return ()
>     sendString remainingMsg = do
>         sent <- sendTo sock remainingMsg sockaddr
>         sendString (genericDrop sent remainingMsg)


> getPacketFromMsg :: String -> Either String GamePacket
> getPacketFromMsg msg = 
>    let gpr = decode msg :: Result GamePacket
>    in case gpr of
>        Ok gp -> Right gp
>        Error err -> Left $ "Error parsing game packet: " ++ err


> registerCallback :: ClientConInfo -> PacketCommand -> PacketCallback -> ClientConInfo
> registerCallback cci command cb = 
>     let cbmap = cciCallbacks cci
>         existingM = DM.lookup command cbmap
>         newcbs = if isNothing existingM
>                    then [cb]
>                    else [cb] ++ (fromJust existingM)
>         newcbmap = DM.insert command newcbs cbmap
>     in cci { cciCallbacks = newcbmap }




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
>        [ ("ReturnPort", showJSON $ gpReturnPort gp)
>        , ("ClientID", showJSON $ gpClientID gp)
>        , ("Seq", showJSON $ gpSeq gp)
>        , ("Command", showJSON $ gpCommand gp)
>        , ("JSONData", showJSON $ gpJSONData gp)
>        ]
>
>    readJSON (JSObject obj) = do
>        let objA = fromJSObject obj
>        rtp <- lookupM "ReturnPort" objA >>= readJSON
>        cid <- lookupM "ClientID" objA >>= readJSON
>        seq <- lookupM "Seq" objA >>= readJSON
>        com <- lookupM "Command" objA >>= readJSON
>        dat <- lookupM "JSONData" objA >>= readJSON
>        return $ GamePacket Nothing rtp cid seq com dat
