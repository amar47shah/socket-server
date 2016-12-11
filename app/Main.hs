import Control.Concurrent (forkFinally)
import Control.Monad (forever)
import GHC.Conc.Sync (ThreadId)
import Network
import System.IO
import Text.Printf

main = withSocketsDo $
  listen port >>= \sock ->
    logListening port *>
      forever
        (accept sock >>= logAcceptance *> communicateOn . fst3)

port :: Int
port = 44444

listen :: Int -> IO Socket
listen = listenOn . PortNumber . fromIntegral

logListening :: Int -> IO ()
logListening = printf "Listening on port %d\n"

logAcceptance :: (PrintfArg b, Show c) => (a, b, c) -> IO ()
logAcceptance (_, h, p) = printf "Accepted connection from %s: %s\n" h $ show p

communicateOn :: Handle -> IO GHC.Conc.Sync.ThreadId
communicateOn = forkFinally <$> talk <*> const . hClose

talk :: Handle -> IO ()
talk h = hSetBuffering h LineBuffering *> loop
    where
  loop = hGetLine h >>= respond
  respond "end" = hPutStrLn h "Thank you for using the Haskell doubling service."
  respond line  = hPutStrLn h (show $ 2 * (read line :: Integer)) *> loop

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
