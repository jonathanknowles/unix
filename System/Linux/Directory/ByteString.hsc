{-# LANGUAGE LambdaCase #-}

#include "HsUnix.h"

{-|
Module      : System.Linux.Directory.ByteString
Description : Offers functionality similar to System.Posix.Directory.ByteString,
              but with Linux-specific extensions.
Stability   : experimental
Portability : Linux
-}

module System.Linux.Directory.ByteString
    ( DirEntryType (..)
    , System.Posix.Directory.ByteString.openDirStream
    , System.Posix.Directory.ByteString.closeDirStream
    , readDirStream ) where

import Foreign
import Foreign.C
import System.Posix.ByteString.FilePath
import System.Posix.Directory.ByteString hiding (readDirStream)
import System.Posix.Directory.Common

import qualified Data.ByteString.Char8 as BC

data DirEntryType
    = File
    | Directory
    | NamedPipe
    | LocalDomainSocket
    | CharacterDevice
    | BlockDevice
    | SymbolicLink
    | Unknown
    deriving (Eq, Show)

decodeDirEntryType :: Word32 -> DirEntryType
decodeDirEntryType = \case
    01 -> NamedPipe
    02 -> CharacterDevice
    04 -> Directory
    06 -> BlockDevice
    08 -> File
    10 -> SymbolicLink
    12 -> LocalDomainSocket
    _  -> Unknown

-- | @readDirStream dp@ calls @readdir@ to obtain the next
--   directory entry (@struct dirent@) for the open directory
--   stream @dp@ and returns the @d_name@ and @d_type@ members
--   of that structure.
readDirStream :: DirStream -> IO (RawFilePath, DirEntryType)
readDirStream (DirStream dirp) =
        alloca $ \ptr_dEnt -> loop ptr_dEnt
    where
        loop ptr_dEnt = do
            resetErrno
            r <- c_readdir dirp ptr_dEnt
            if (r == 0)
                then do
                    dEnt <- peek ptr_dEnt
                    if (dEnt == nullPtr)
                        then return (BC.empty, Unknown)
                        else do
                            dname <- (d_name dEnt >>= peekFilePath)
                            dtype <- (d_type dEnt)
                            c_freeDirEnt dEnt
                            return (dname, decodeDirEntryType dtype)
                else do
                    errno <- getErrno
                    if (errno == eINTR)
                        then loop ptr_dEnt
                        else do
                            let (Errno eo) = errno
                            if (eo == 0)
                                then return (BC.empty, Unknown)
                                else throwErrno "readDirStream"

foreign import ccall unsafe "__hscore_readdir"
    c_readdir  :: Ptr CDir -> Ptr (Ptr CDirent) -> IO CInt
foreign import ccall unsafe "__hscore_free_dirent"
    c_freeDirEnt  :: Ptr CDirent -> IO ()
foreign import ccall unsafe "__hscore_d_name"
    d_name :: Ptr CDirent -> IO CString
foreign import ccall unsafe "__hscore_d_type"
    d_type :: Ptr CDirent -> IO Word32

