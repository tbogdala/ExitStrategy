Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module Server where

The code for the multiplayer server. Coordinates the games
for both player vs AI and player vs player.

> import Data.Maybe
> import Network.Socket
> import System.IO
> import Control.Concurrent
> import Control.Concurrent.STM
> import Control.Monad
> import Control.Monad.IO.Class (liftIO)
> import Control.Monad.Trans.Class (lift)
> import qualified Data.Map as DM
> import qualified Control.Monad.Trans.State.Lazy as MTS

> import Utils
> import qualified GamePacket as GP


> type ServerStateIO a = MTS.StateT GameData IO a

> data GameData = GameData
>     {
>         gdIncPacketsTChan :: IncGamePacketChan,
>         gdPlayers :: ClientMap,
>         gdGameState :: GameState
>     }

> data GameState = NoGame 
>                | InitializingGame
>                | GameRunning
>                | GameOver
>    deriving (Show, Eq, Enum, Ord)


> type ClientMap = TVar (DM.Map Int (GP.ClientConInfo, TChan GP.GamePacket))
> type IncGamePacketChan = TChan (Int, GP.GamePacket)


Initializes the main game server and sets up the network listening socket
on the port specified. 

Designed to run in its own thread.

> gameServer :: Int -> IO ()
> gameServer port = do
>     putStrLn "Initializing game server ..."
>     (runServer port) `catch`
>         (\e -> (putStrLn $ "ERROR: Failed to listen on socket for clients! " ++ 
>                    show e) >> return ())



> runServer :: Int -> IO ()
> runServer port = do
>     incomingTC <-  newTChanIO
>     clients <- newTVarIO $ DM.empty
>     addrInfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
>                              Nothing (Just $ show port)
>     let serveraddr = head addrInfos
>     sock <- socket (addrFamily serveraddr) Stream defaultProtocol 
>     bindSocket sock (addrAddress serveraddr)
>     listen sock 5
>     forkIO $ processIncomingCon sock 1 clients incomingTC
>
>     MTS.evalStateT gameServerLoop $ GameData incomingTC clients NoGame


> processIncomingCon :: Socket ->               -- listening socket
>                       Int ->                  -- next client id
>                       ClientMap ->            -- client map
>                       IncGamePacketChan ->    -- channel for incoming GP
>                       IO ()
> processIncomingCon masterSock nextID clients incomingTC = do
>     (conSock, conAddr) <- accept masterSock
>     h <- socketToHandle conSock ReadWriteMode
>     hSetBuffering h LineBuffering
>     let cci = GP.ClientConInfo h conAddr "" DM.empty
>     outgoingTC <- newTChanIO 
>     atomically $ modifyTVar_ clients (DM.insert nextID (cci, outgoingTC))

>     forkIO $ clientThread cci nextID h outgoingTC incomingTC
>     processIncomingCon masterSock (nextID + 1) clients incomingTC


> clientThread :: GP.ClientConInfo ->
>                 Int ->
>                 Handle -> 
>                 TChan GP.GamePacket -> 
>                 IncGamePacketChan -> 
>                 IO ()
> clientThread cci clientId h outgoingTC incomingTC = do
>     forever loop 
>  where
>   loop = do
>     incomingMsg <- GP.readPacket cci
>     when (isJust incomingMsg) $ sendPacketUpstream (fromJust incomingMsg)
>     noOutgoingMsg <- atomically $ isEmptyTChan outgoingTC
>     when (not noOutgoingMsg) writeMsg
>     yield

>   sendPacketUpstream gp = 
>         atomically $ writeTChan incomingTC (clientId, gp)

>   writeMsg = do
>     packet <- atomically $ readTChan outgoingTC
>     GP.sendPacket cci packet

Will return immediately if it cannot creat the listening socket.

 gameServer :: Int -> IO ()
 gameServer port = 
    (do ch <-GPL.makeListener port
        putStrLn "gameServer initialized; socket ready for listening."
        MTS.evalStateT gameServerLoop $ initialState ch) `catch`
        (\e -> do putStrLn $ "Failed to make network listing socket! : " ++ show e
                  return ())


> gameServerLoop :: ServerStateIO ()
> gameServerLoop = do
>     gd <- MTS.get
>     let ch = gdIncPacketsTChan gd
>     emptyCh <- liftIO $ atomically $ isEmptyTChan ch
>     if emptyCh
>         then do liftIO $ yield
>         else do (clientId, gp) <- liftIO $ atomically $ readTChan ch
>                 handlePacket clientId gp
>     gameServerLoop



> handlePacket :: Int -> GP.GamePacket -> ServerStateIO ()
> handlePacket clientId gp = do 
>    let cmd = GP.gpCommand gp
>    case cmd of
>        GP.InitGameReq ->  do
>            gd <- MTS.get
>            if gdGameState gd == NoGame
>              then do
>                MTS.put $ gd { gdGameState = InitializingGame }
>                sendResponse clientId GP.InitGameResp "True"
>              else do
>                sendResponse clientId GP.InitGameResp "False"
>        _ -> do return ()



> sendResponse :: Int -> GP.PacketCommand -> String -> ServerStateIO ()
> sendResponse clientId command json = do
>     gd <- MTS.get
>     playerMap <- liftIO $ atomically $ readTVar $ gdPlayers gd
>     let playerM = DM.lookup clientId playerMap
>     case playerM of
>       Nothing -> do liftIO $ putStrLn $ "ERROR: (sendResponse) unknown clientId on server: "
>                               ++ show clientId
>                     return ()
>       Just (_ , clientChan) -> do
>           let packet = GP.createNewPacket command json
>           liftIO $ atomically $ writeTChan clientChan packet
>           liftIO $ putStrLn "Sent response msg."








 

