{-# LANGUAGE OverloadedStrings #-}
-- vim: sw=4

import Control.Applicative ((<$>))
import Control.Error (Script, runScript, scriptIO, headDef, left, right, tryIO, eitherT, runEitherT)
import Network.Wai (pathInfo, Application, responseFile)
import BetterNetwork (PortNumber, Socket, socketPort, withSockets, getListeningLocalSocket)
import qualified Network.Wai.Handler.Warp as Warp
import Network.HTTP.Types.Status (status200)
import Network.Mime (defaultMimeLookup)
import Control.Concurrent.Async (Async, withAsync, waitEither_)
import qualified Data.Text as T
import System.Directory (doesFileExist, doesDirectoryExist)
import System.Process (callProcess)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (exitFailure)
import System.FilePath ((</>), replaceFileName)
import qualified LauncherConfig as Config

-- Gracefully exit by printing the first error.
main = logScript "linkbotlog.txt" $ do
    browser <- processCfg doesFileExist Config.browserExecutablePath
    path' <- scriptIO $ headDef Config.contentPath <$> getArgs
    content <- processCfg doesDirectoryExist path'

    -- Henceforth, only IO Exceptions will catch us up; thus one scriptIO
    -- to catch 'em all.
    scriptIO $ withSockets $ do
        (port, withAsyncSrv) <- spawnServer content
        withAsyncSrv $ \asyncSrv -> do
        withAsync (runBrowser browser port) $ \asyncBrowser -> do
        waitEither_ asyncSrv asyncBrowser

-- TODO combine these two?
processCfg test path = do
    out <- mungePath path
    pathErr test "Could not find " out
  where
    mungePath b = scriptIO $ do
        epath <- getExecutablePath
        return $ replaceFileName epath b

    pathErr test err path = do
        b <- scriptIO $ test path
        if b
           then right path
           else left $ err ++ show path

logScript :: FilePath -> Script a -> IO a
logScript log s = do
    e <- runEitherT s
    case e of
        Left  e -> do
            writeFile log e
            exitFailure
        Right a -> return a


--
-- -- Server stuff --
--

-- Using continuations, since that's what withAsync uses.
spawnServer :: FilePath
            -> IO (PortNumber, ((Async () -> IO b) -> IO b))
spawnServer dir = do
  sock <- (eitherT error return) $ getListeningLocalSocket $ Config.port
  actualPort <- socketPort sock
  return (actualPort, withAsync $ startServer sock dir)

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

--
-- -- Browser stuff --
--

runBrowser :: FilePath -> PortNumber -> IO ()
runBrowser browser port = callProcess browser [url]
  where url = "http://localhost:" ++ show port ++ "/index.html"
