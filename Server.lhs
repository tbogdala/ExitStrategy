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

> type ServerStateIO a = MTS.StateT GameData IO a

> initialState ch = GameData ch NoGame []


Will return immediately if it cannot creat the listening socket.

> gameServer :: String -> IO ()
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
>                 let Just (SockAddrInet port host) = GP.gpSockAddr gp
>                 liftIO $ putStrLn $ "PM= Got a new packet: " ++ show host ++ " port " ++ show port
>                 sendPacketAck gp
>                 handlePacket gp
>     gameServerLoop

> sendPacketAck :: GamePacket -> ServerStateIO ()
> sendPacketAck gp = do
>     let sockAddrM = GP.gpSockAddr gp
>     if isNothing sockAddrM
>         then liftIO $ putStrLn "ERROR: Tried to ACK a GamePacket with no SockAddr." >> return ()
>         else do
>             sock <- liftIO $ socket AF_INET Datagram defaultProtocol
>             let (_, ackPack) = GP.createNewPacket Nothing GP.ACK $ "ACK " ++ show (gpSeq gp)
>             liftIO $ GP.sendAnonyPacket sock (fromJust sockAddrM) ackPack

> handlePacket :: GamePacket -> ServerStateIO ()
> handlePacket gp = do return ()





