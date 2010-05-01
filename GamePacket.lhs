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

> import Utils

> currentProtocolID = 1
> defaultPort = "45954"
> defaultPortNum = 45954
> maxPacketSize = 1024 :: Int

> data ClientConInfo = ClientConInfo
>     {
>         cciSocket :: Socket,
>         cciAddress :: SockAddr,
>         cciClientName :: String,
>         cciClientID :: Int,
>         cciLastSeq :: Int
>     }

> data GamePacket = GamePacket
>     {
>         gpSockAddr :: Maybe SockAddr,
>         gpClientID :: Int,
>         gpSeq :: Int,
>         gpCommand :: PacketCommand,
>         gpJSONData :: String
>     }

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
>    deriving (Eq, Show, Enum)


> data GPBoolResp = GPBoolResp Bool
> instance JSON GPBoolResp where
>    showJSON (GPBoolResp br) = makeObj
>        [ ("Response", showJSON br) ]
>
>    readJSON (JSObject obj) = do
>        let objA = fromJSObject obj
>        br <- lookupM "Response" objA >>= readJSON
>        return $ GPBoolResp br



> createNewPacket :: Maybe ClientConInfo -> PacketCommand -> String ->
>                         (Maybe ClientConInfo, GamePacket)
> createNewPacket cciM pc json = 
>     let lastSeq = if isNothing cciM then (-1) else cciLastSeq cci
>         newCCI = if isNothing cciM then Nothing else Just $ cci { cciLastSeq = lastSeq + 1 }
>         cci = fromJust cciM
>         clientId = if isNothing cciM then 0 else cciClientID cci
>         gp = GamePacket Nothing clientId (lastSeq+1) pc json
>     in (newCCI, gp)


 createNewCCI :: ClientConInfo
 createNewCCI = ClientConInfo Nothing Nothing [] (-1) (-1)

Actual networking is disabled for now.

> openServerConnection :: HostName -> String -> IO ClientConInfo
> openServerConnection hostname port = do
>     addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
>     let serveraddr = head addrinfos
>     sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
>     connect sock (addrAddress serveraddr)
>     return $ ClientConInfo sock (addrAddress serveraddr) 
>                  [] (-1) (-1)



> sendPacket :: ClientConInfo -> GamePacket -> IO ()
> sendPacket cci gp = do
>     sendAnonyPacket (cciSocket cci) (cciAddress cci) gp


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
>        [ ("ClientID", showJSON $ gpClientID gp)
>        , ("Seq", showJSON $ gpSeq gp)
>        , ("Command", showJSON $ gpCommand gp)
>        , ("JSONData", showJSON $ gpJSONData gp)
>        ]
>
>    readJSON (JSObject obj) = do
>        let objA = fromJSObject obj
>        cid <- lookupM "ClientID" objA >>= readJSON
>        seq <- lookupM "Seq" objA >>= readJSON
>        com <- lookupM "Command" objA >>= readJSON
>        dat <- lookupM "JSONData" objA >>= readJSON
>        return $ GamePacket Nothing cid seq com dat
