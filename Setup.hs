import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (hostPlatform)
import Distribution.System

import System.Process (callProcess)

main = defaultMainWithHooks simpleUserHooks { buildHook = compileIcon }

compileIcon _pkgDescr buildInfo _hooks _flags =
  let (Platform _ host) = hostPlatform buildInfo
  in do
    case host of
        Windows ->
          callProcess "windres" ["-i", "icon.rc",
                                 "-o", "icon.o"]
        _ -> return ()
    -- The default build hook is the actual build process, so go ahead and
    -- run it! (Is there a better way?)
    (buildHook simpleUserHooks) _pkgDescr buildInfo _hooks _flags
