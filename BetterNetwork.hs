-- | A wrapper for Network.Socket that has tolerably sensible functions
module BetterNetwork (
  -- * Some friendlier high-level stuff
  getListeningLocalSocket
  -- * Wrapper over the low level
  , module Network.Socket
  , getAddrInfo
  , socket
  , setSocketOption
  , bind
  , listen
  , withSockets
) where

import Control.Error
import Control.Exception

import System.IO.Error (ioeGetErrorString)

import Network.Socket hiding (getAddrInfo, socket, bind, listen, setSocketOption, withSocketsDo)
import qualified Network.Socket as N

type SockOpts = [(SocketOption, Int)]

-- | Better name for 'withSocketsDo', which is usually followed by "$ do".
-- That is, dare I say, doo doo.
withSockets :: IO a -> IO a
withSockets = N.withSocketsDo

-- | Given a service (port), return a listening socket on the loopback
-- device.
getListeningLocalSocket :: String -> EitherT String IO Socket
getListeningLocalSocket service =
 let addrInfoReq = Just $ defaultHints { addrSocketType = Stream }
     sOpts = [(ReuseAddr, 1)]
     host = Just "localhost"
  in
    do
      addrs <- ioeStrT $ getAddrInfo addrInfoReq host (Just service)
      sock <- noteT "No sockets for listening" $ getSocket addrs sOpts
      ioeStrT $ listen sock 10
      return sock
  where
   ioeStrT = fmapLT ioeGetErrorString

getSocket :: [AddrInfo] -> SockOpts -> MaybeT IO Socket
getSocket ais sOpts = firstSuccessT (boundSocket sOpts) ais

boundSocket :: SockOpts -> AddrInfo -> EitherT IOException IO Socket
boundSocket sOpts ai = do
  -- Make a socket
  sock <- socket ai
  -- Set options
  let setter = setSocketOption sock
      setAll = sequence_ . map (uncurry setter)
  setAll sOpts
  -- Bind it
  bind sock ai
  -- Success!
  return sock

-- | A somewhat generic method building on the errors library.
-- Given a function (kleisli arrow) and a list of inputs, apply the
-- function and sequence the resulting actions, short-circuiting after the
-- first success.
firstSuccessT :: Monad m
              => (i -> EitherT e m o) -- ^ function that can error
              -> [i]                   -- ^ bunch of inputs for function
              -> MaybeT m o            -- ^ first non-error result
firstSuccessT f = hushT . runEitherRT . sequence . map (EitherRT . f)


--
-- The low level stuff
--

type EitherIOE = EitherT IOException IO

setSocketOption :: Socket
                -> SocketOption -- Option Name
                -> Int          -- Option Value
                -> EitherIOE ()
setSocketOption s n v = tryIO $ N.setSocketOption s n v

getAddrInfo :: Maybe AddrInfo -- ^ preferred socket type or protocol
            -> Maybe HostName -- ^ host name to look up
            -> Maybe ServiceName -- ^ service name to look up
            -> EitherIOE [AddrInfo] -- ^ resolved addresses, with "best" first
getAddrInfo a h s = tryIO $ N.getAddrInfo a h s

socket :: AddrInfo -> EitherIOE Socket
socket (AddrInfo _ fam sockType protoNum _ _) = tryIO $ N.socket fam sockType protoNum

bind :: Socket -> AddrInfo -> EitherIOE ()
bind s ai = tryIO $ N.bind s (addrAddress ai)

listen :: Socket -> Int -> EitherIOE ()
listen s = tryIO . N.listen s
