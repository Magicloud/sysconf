module System.Posix.SysConf where

#include <unistd.h>
import Data.Int
import Foreign.C.Error

{#enum _SC_ARG_MAX as SysConf {underscoreToCase}#}

data SCStatus = SystemError Errno
              | NotSupportedOrIndeterminate

sysconf :: SysConf -> IO (Either SCStatus Integer)
sysconf sc = do
  resetErrno
  ret <- fmap fromIntegral $ sysconf_ (fromIntegral $ fromEnum sc)
  errno <- getErrno
  if ret == -1
    then if errno == eOK
    then return $ Left NotSupportedOrIndeterminate
    else return $ Left $ SystemError errno
    else return $ Right ret
  where
    {#fun sysconf as sysconf_ { `Int32' } -> `Int64'#}
