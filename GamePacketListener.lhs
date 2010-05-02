Copyright (c) 2010 Timothy Bogdala (http://www.animal-machine.com)
GPL version 3 or later (see http://www.gnu.org/licenses/gpl.html)

> module GamePacketListener where

The network listening routines that setup a network
sock and pass the parsed game packets over a Chan.

> import Network.Socket
> import Network.BSD
> import System.IO
> import Control.Concurrent
> import Control.Concurrent.Chan
> import Control.Monad

> import GamePacket as GP

Creates a network UDP socket, for the port requested, on the client
machine. This socket is just for listing for incoming packets.

May throw errors!

> makeListener :: Int -> IO (Chan GamePacket)
> makeListener port = do
>     addrinfos <- getAddrInfo
>                  (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
>                  Nothing 
>                  (Just $ show port)
>     let serveraddr = head addrinfos
>     sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
>     bindSocket sock (addrAddress serveraddr)
>     ch <- newChan
>     forkIO $ gamePacketListener sock ch
>     return ch

> gamePacketListener :: Socket -> Chan GamePacket -> IO ()
> gamePacketListener s c = do
>     forever $ readPacket s c
>   where 
>     readPacket s c = do
>         (msg, _, addr) <- recvFrom s maxPacketSize
>         putStrLn $ "GPL=(From: " ++ (show addr) ++ ") " ++  msg
>         let egp = getPacketFromMsg msg
>         case egp of
>             Left err -> (putStrLn $ "GPL= Error getting packet! " ++ err) >> return ()
>             Right gp -> writeChan c $ gp { gpSockAddr = Just addr }
