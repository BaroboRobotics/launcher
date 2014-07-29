{-# LANGUAGE OverloadedStrings #-}
-- vim: sw=4

import Control.Error
import Control.Exception
import Control.Monad (join)
import Network.Wai
import BetterNetwork
import qualified Network.Wai.Handler.Warp as Warp
import Network.HTTP.Types.Status
import Network.Mime (defaultMimeLookup)
import System.FilePath ((</>))
import Control.Concurrent
import qualified Data.Text as T
import System.IO.Unsafe
import System.IO.Error (ioeGetErrorString)
import System.Process
import System.Environment
import System.Exit
import qualified LauncherConfig as Config

-- This stuff is basically just copied from
-- http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent.html

trackedChildren :: MVar [MVar ()]
trackedChildren = unsafePerformIO (newMVar [])

waitForTrackedChildren :: IO ()
waitForTrackedChildren = do
  cs <- takeMVar trackedChildren
  case cs of
    []   -> return ()
    m:ms -> do
      putMVar trackedChildren ms
      takeMVar m
      waitForTrackedChildren

forkTrackedChild :: IO () -> IO ThreadId
forkTrackedChild io = do
  mvar <- newEmptyMVar
  childs <- takeMVar trackedChildren
  putMVar trackedChildren (mvar:childs)
  forkFinally io (\_ -> putMVar mvar ())



linkbotLabs :: FilePath -> IO ()
linkbotLabs directory = withSocketsDo $ do
  (port, serverThread) <- forkServer directory
  browserThread <- forkTrackedChild $ startBrowser port
  return ()

forkServer :: FilePath -> IO (PortNumber, ThreadId)
forkServer dir = do
  sock <- (eitherT error return) $ getListeningLocalSocket $ Config.port
  actualPort <- socketPort sock
  threadId <- forkIO $ startServer sock dir
  return (actualPort, threadId)

startServer :: Socket -> FilePath -> IO ()
startServer sock dir = do
    Warp.runSettingsSocket settings sock app
  where
    app :: Application -- aka Request -> (Reponse -> IO ReponseReceived) -> IO ResponseReceived
    app req func =
      let path = foldl (</>) dir $ map T.unpack $ pathInfo req
          mimeHeader = ("Content-Type", defaultMimeLookup $ T.pack path)
      in
        func $ responseFile status200 [mimeHeader] path Nothing
    settings = Warp.defaultSettings


startBrowser :: PortNumber -> IO ()
startBrowser port = do
    putStrLn $ "Starting browser with url: " ++ url
    exitCode <- rawSystem Config.browserExecutablePath [url]
    if exitCode == ExitSuccess 
        then 
            return ()
        else 
            putStrLn $ "Error: Could not find browser at " ++ Config.browserExecutablePath
  where url = "http://localhost:" ++ show port ++ "/index.html"

main = do
  contentPath <- fmap (headDef Config.contentPath) getArgs
  linkbotLabs contentPath
