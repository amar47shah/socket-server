module Main where

import qualified Data.Map as Map

import Control.Concurrent (ThreadId, forkFinally)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import Control.Concurrent.STM.TVar (TVar, newTVar, newTVarIO, readTVar, writeTVar, modifyTVar')
import Control.Exception (finally, mask)
import Control.Monad (forever, join, void, when)
import Control.Monad.STM (STM, atomically)
import Data.Map (Map)
import Network (HostName, PortID (PortNumber), PortNumber, Socket, accept, listenOn, withSocketsDo)
import System.IO (Handle, BufferMode (LineBuffering), hClose, hGetLine, hPutStrLn, hSetBuffering, hSetNewlineMode, universalNewlineMode)
import Text.Printf (hPrintf, printf)

main :: IO ()
main = withSocketsDo $ do
  server <- newServer
  socket <- listenOn' port
  forever $ connect server socket

connect :: Server -> Socket -> IO ThreadId
connect server sock = accept' sock >>= forkServer server

forkServer :: Server -> Handle -> IO ThreadId
forkServer s = forkFinally <$> serve s <*> const . hClose

-- ---------------------------------------------------------------------------
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

-- ---------------------------------------------------------------------------
-- Data structures

type ClientName = String

data Client = Client
  { clientName   :: ClientName
  , clientHandle :: Handle
  , clientKicked :: TVar (Maybe String)
  , clientDM     :: TChan Message
  }

newClient :: ClientName -> Handle -> STM Client
newClient n h = Client n h <$> newTVar Nothing <*> newTChan

data Server = Server { clients :: TVar (Map ClientName Client) }

newServer :: IO Server
newServer = Server <$> newTVarIO Map.empty

data Message = Notice String
             | Tell ClientName String
             | Broadcast ClientName String
             | Command String

-- -----------------------------------------------------------------------------
-- Basic operations

broadcast :: Server -> Message -> STM ()
broadcast s m = readTVar (clients s) >>= mapM_ (`sendMessage` m) . Map.elems

sendMessage :: Client -> Message -> STM ()
sendMessage = writeTChan . clientDM

sendToName :: Server -> ClientName -> Message -> STM Bool
sendToName s n m =
  readTVar (clients s) >>= \cs ->
  case Map.lookup n cs of
    Nothing -> pure False
    Just c  -> const True <$> c `sendMessage` m

tell :: Server -> Client -> ClientName -> String -> IO ()
tell s from nameTo m =
   atomically (sendToName s nameTo $ Tell (clientName from) m) >>= \ok ->
     if ok
     then pure ()
     else hPutStrLn (clientHandle from) $ nameTo ++ " is not connected."

kick :: Server -> ClientName -> ClientName -> STM ()
kick s nameWho by = do
  cs <- readTVar $ clients s
  case Map.lookup nameWho cs of
    Nothing -> void . sendToName s by . Notice $ nameWho ++ " is not connected"
    Just k  -> do
      writeTVar (clientKicked k) $ Just ("by " ++ by)
      void $ sendToName s by . Notice $ "you kicked " ++ nameWho

-- -----------------------------------------------------------------------------
-- The main server

serve :: Server -> Handle -> IO ()
serve server handle = do
  hSetNewlineMode handle universalNewlineMode
      -- Swallow carriage returns sent by telnet clients
  hSetBuffering handle LineBuffering
  getName server handle

getName :: Server -> Handle -> IO ()
getName server handle =
  hPutStrLn handle "What is your name?" *>
    readName server handle

readName :: Server -> Handle -> IO ()
readName server handle =
  hGetLine handle >>= \name ->
  case name of
    "" -> readName server handle
    n  -> mask $ \restore ->
      checkAddClient server n handle >>= \ok ->
        case ok of
          Nothing -> restore $ hPrintf handle "The name %s is in use, please choose another\n" name *> readName server handle
          Just client -> restore (runClient server client) `finally` removeClient server name

checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server name handle = atomically $ do
  cs <- readTVar $ clients server
  if Map.member name cs
    then pure Nothing
    else do client <- newClient name handle
            writeTVar (clients server) $ Map.insert name client cs
            broadcast server  $ Notice (name ++ " has connected")
            pure (Just client)

removeClient :: Server -> ClientName -> IO ()
removeClient server name = atomically $ do
  modifyTVar' (clients server) $ Map.delete name
  broadcast server $ Notice (name ++ " has disconnected")

runClient :: Server -> Client -> IO ()
runClient s = race_ <$> respond s <*> receive

receive :: Client -> IO a
receive c = forever $
  hGetLine (clientHandle c) >>=
    atomically . sendMessage c . Command

respond :: Server -> Client -> IO ()
respond serv c = join . atomically $
  readTVar (clientKicked c) >>= \k ->
  case k of
    Just reason -> pure . hPutStrLn (clientHandle c) $ "You have been kicked: " ++ reason
    Nothing -> do
      msg <- readTChan (clientDM c)
      pure $ do
        continue <- handleMessage serv c msg
        when continue $ respond serv c

handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage _ c (Notice msg)         = output c $ "*** " ++ msg
handleMessage _ c (Tell name msg)      = output c $ "*" ++ name ++ "*: " ++ msg
handleMessage _ c (Broadcast name msg) = output c $ "<" ++ name ++ ">: " ++ msg
handleMessage s c (Command msg) = case words msg of
  "/kick" : whom : []   -> const True <$> (atomically $ kick s whom $ clientName c)
  "/tell" : whom : what -> const True <$> tell s c whom (unwords what)
  "/quit" : []          -> pure False
  ('/':_) : _           -> const True <$> (hPutStrLn (clientHandle c) $ "Unrecognised command: " ++ msg)
  _                     -> const True <$> (atomically . broadcast s $ Broadcast (clientName c) msg)

output :: Client -> String -> IO Bool
output c s = const True <$> hPutStrLn (clientHandle c) s
