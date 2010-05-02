Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module Server where

The code for the multiplayer server. Coordinates the games
for both player vs AI and player vs player.

> import Data.Maybe
> import Network.Socket
> import Network.BSD
> import System.IO
> import Control.Concurrent
> import Control.Concurrent.Chan
> import Control.Monad
> import qualified Control.Monad.Trans.State.Lazy as MTS
> import Control.Monad.IO.Class (liftIO)
> import Control.Monad.Trans.Class (lift)

> import GamePacket as GP
> import GamePacketListener as GPL


> data GameData = GameData
>     {
>         gdPacketChan :: Chan GamePacket,
>         gdGameState :: GameState,
>         gdPlayers :: [GP.ClientConInfo]
>     }

> data GameState = NoGame 
>                | InitializingGame
>                | GameRunning
>                | GameOver
>    deriving (Show, Eq, Enum, Ord)

> type ServerStateIO a = MTS.StateT GameData IO a

> initialState ch = GameData ch NoGame []


Will return immediately if it cannot creat the listening socket.

> gameServer :: Int -> IO ()
> gameServer port = 
>    (do ch <-GPL.makeListener port
>        putStrLn "gameServer initialized; socket ready for listening."
>        MTS.evalStateT gameServerLoop $ initialState ch) `catch`
>        (\e -> do putStrLn $ "Failed to make network listing socket! : " ++ show e
>                  return ())


> gameServerLoop :: ServerStateIO ()
> gameServerLoop = do
>     gd <- MTS.get
>     let ch = gdPacketChan gd
>     emptyCh <- liftIO $ isEmptyChan ch
>     if emptyCh
>         then do liftIO $ yield
>         else do gp <- liftIO $ readChan ch
>                 sendPacketAck gp
>                 handlePacket gp
>     gameServerLoop

FIXME: might crash on call to head

> sendPacketAck :: GamePacket -> ServerStateIO ()
> sendPacketAck gp = do
>    let seq = gpSeq gp
>    sendResponse gp seq GP.ACK $ "ACK " ++ show seq


> handlePacket :: GamePacket -> ServerStateIO ()
> handlePacket gp = do 
>    gs <- MTS.get
>    let cmd = gpCommand gp
>    let seq = gpSeq gp
>    case cmd of
>        InitGameReq -> 
>            if gdGameState gs == NoGame
>              then do
>                MTS.put $ gs { gdGameState = InitializingGame }
>                sendResponse gp seq GP.InitGameResp "True"
>              else do
>                sendResponse gp seq GP.InitGameResp "False"
>        _ -> do return ()


> sendResponse :: GamePacket -> Int -> GP.PacketCommand -> String -> ServerStateIO ()
> sendResponse gp seq command json = do
>   if GP.gpReturnPort gp == 0
>    then return ()
>    else do
>     let sockAddrM = GP.gpSockAddr gp
>     if isNothing sockAddrM
>         then liftIO $ putStrLn "ERROR: Tried to respond to a GamePacket with no SockAddr." >> return ()
>         else do
>             let (SockAddrInet _ addr) = fromJust sockAddrM
>             hostStr <- liftIO $ inet_ntoa addr
>             addrinfos <- liftIO $ getAddrInfo Nothing 
>                                               (Just hostStr) 
>                                               (Just $ show (GP.gpReturnPort gp))
>             let returnSockAddr = addrAddress $ head addrinfos

>             sock <- liftIO $ socket AF_INET Datagram defaultProtocol
>             let (_, ackPack) = GP.createNewPacket Nothing 
>                                     GP.defaultPortNum 
>                                     command 
>                                     json
>             liftIO $ GP.sendAnonyPacket sock returnSockAddr (ackPack { gpSeq = seq })
>             liftIO $ putStrLn $ "Sent response to " ++ show returnSockAddr








 

