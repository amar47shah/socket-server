import Control.Applicative (liftA2)
import Control.Concurrent (ThreadId, forkFinally)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Monad (forever, join, void)
import Control.Monad.STM (STM, atomically)
import Network (HostName, PortID (PortNumber), PortNumber, Socket, accept, listenOn, withSocketsDo)
import System.IO (Handle, BufferMode (LineBuffering), hClose, hGetLine, hPutStrLn, hSetBuffering)
import Text.Printf (hPrintf, printf)

type TFactor = TVar Integer

main :: IO ()
main = withSocketsDo $ do
  socket  <- listenOn' port
  tFactor <- atomically initializeTFactor
  forever $ connect socket tFactor

connect :: Socket -> TFactor -> IO ThreadId
connect s tf = accept' s >>= forkServer tf

-- Networking

port :: PortID
port = PortNumber 44444

listenOn' :: PortID -> IO Socket
listenOn' p = listenOn p >>= \socket -> logListening p *> pure socket

accept' :: Socket -> IO Handle
accept' s = accept s >>= \(handle, h, p) -> logAcceptance h p *> pure handle

logListening :: PortID -> IO ()
logListening = printf "Listening on port %s\n" . show

logAcceptance :: HostName -> PortNumber -> IO ()
logAcceptance h p = printf "Accepted connection from %s: %s\n" h $ show p

-- Serving

initializeTFactor :: STM (TFactor)
initializeTFactor = newTVar 2

forkServer :: TFactor -> Handle -> IO ThreadId
forkServer tf = forkFinally <$> serve tf <*> const . hClose

serve :: TFactor -> Handle -> IO ()
serve tf h =
  hSetBuffering h LineBuffering *>
    atomically newTChan >>=
      void . (race <$> respond tf h <*> receive h)

receive :: Handle -> TChan String -> IO ()
receive h c = forever $ hGetLine h >>= atomically . writeTChan c

respond :: TFactor -> Handle -> TChan String -> IO ()
respond tf h c =
  atomically (readTVar tf) >>=
    liftA2 (*>) (hPrintf h "Starting factor: %d\n") (loop tf h c)

loop :: TFactor -> Handle -> TChan String -> Integer -> IO ()
loop tf h c = join . atomically . react tf h c

react :: TFactor -> Handle -> TChan String -> Integer -> STM (IO ())
react tf h c f =
  readTVar tf >>= \fNow ->
    if f == fNow
    then command tf h c f <$> readTChan c
    else pure $ hPrintf h "New factor: %d\n" fNow *> loop tf h c fNow

command :: TFactor -> Handle -> TChan String -> Integer -> String -> IO ()
command _  h _ _ "end"   = end h
command tf h c f ('*':s) = changeFactor tf s *> loop tf h c f
command tf h c f s       = multiply h f    s *> loop tf h c f

changeFactor :: TFactor -> String -> IO ()
changeFactor tf = atomically . writeTVar tf . readInteger

multiply :: Handle -> Integer -> String -> IO ()
multiply h f = hPutStrLn h . show . (f *) . readInteger

end :: Handle -> IO ()
end h = hPutStrLn h "Thank you for using the Haskell multiplying service."

readInteger :: String -> Integer
readInteger = read
