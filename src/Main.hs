{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (zip)

import qualified Codec.Archive.Zip as Zip
import Control.Exception.Safe (handleIO)
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as B
import Data.Traversable (forM)
import qualified GHC.IO.Exception as GIE
import qualified System.FilePath as Path
import System.FilePath
  ( addTrailingPathSeparator
  , makeRelative
  , replaceExtensions
  , takeDirectory
  , takeExtensions
  )
import qualified System.Fuse as Fuse
import System.Fuse
  ( Errno(..)
  , FileStat(..)
  , FuseOperations
  , OpenFileFlags
  , OpenMode
  , defaultExceptionHandler
  , fuseMain
  , unionFileModes
  )
import qualified System.Posix.Directory as Dir
import qualified System.Posix.Files as File
import System.Posix.Types (ByteCount, EpochTime, FileOffset)

main :: IO ()
main = fuseMain fuseOps defaultExceptionHandler

type FHandle = ()

data ZipPath = NormalPath FilePath
             | ZipPath FilePath FilePath
             deriving (Eq, Show)

isPseudoZipDir :: FilePath -> Bool
isPseudoZipDir = (== ".zip.d") . takeExtensions

toPseudoZipDir :: FilePath -> FilePath
toPseudoZipDir = (flip replaceExtensions) "zip.d"

fromPseudoZipDir :: FilePath -> FilePath
fromPseudoZipDir = (flip replaceExtensions) "zip"

isZipFile :: FilePath -> Bool
isZipFile = (== ".zip") . takeExtensions

isDirPath :: FilePath -> Bool
isDirPath path = path == addTrailingPathSeparator path

prefixes :: FilePath -> [FilePath]
prefixes = reverse . takeWhile reducible . iterate takeDirectory
  where reducible dir = dir /= takeDirectory dir

fromFilePath :: FilePath -> ZipPath
fromFilePath path =
  case ziproot of
    Just zipdir -> ZipPath zipfile relfile
      where
        zipfile = fromPseudoZipDir zipdir
        relfile = makeRelative zipdir path
    _ -> NormalPath path
  where
    ziproot :: Maybe FilePath
    ziproot =
      case (filter isPseudoZipDir $ prefixes path) of
        (zipdir:_) -> Just zipdir
        _ -> Nothing

-- identical to defaultFuseOps, but here for reference
fuseOps :: FuseOperations FHandle
fuseOps = Fuse.FuseOperations
  { Fuse.fuseGetFileStat = fuseGetFileStat
  , Fuse.fuseReadSymbolicLink = \_ -> return (Left Fuse.eNOSYS)
  , Fuse.fuseCreateDevice = \_ _ _ _ ->  return Fuse.eNOSYS
  , Fuse.fuseCreateDirectory = \_ _ -> return Fuse.eNOSYS
  , Fuse.fuseRemoveLink = \_ -> return Fuse.eNOSYS
  , Fuse.fuseRemoveDirectory = \_ -> return Fuse.eNOSYS
  , Fuse.fuseCreateSymbolicLink = \_ _ -> return Fuse.eNOSYS
  , Fuse.fuseRename = \_ _ -> return Fuse.eNOSYS
  , Fuse.fuseCreateLink = \_ _ -> return Fuse.eNOSYS
  , Fuse.fuseSetFileMode = \_ _ -> return Fuse.eNOSYS
  , Fuse.fuseSetOwnerAndGroup = \_ _ _ -> return Fuse.eNOSYS
  , Fuse.fuseSetFileSize = \_ _ -> return Fuse.eNOSYS
  , Fuse.fuseSetFileTimes = \_ _ _ -> return Fuse.eNOSYS
  , Fuse.fuseOpen = fuseOpen
  , Fuse.fuseRead = fuseRead
  , Fuse.fuseWrite =  \_ _ _ _ -> return (Left Fuse.eNOSYS)
  , Fuse.fuseGetFileSystemStats = \_ -> return (Left Fuse.eNOSYS)
  , Fuse.fuseFlush = \_ _ -> return Fuse.eOK
  , Fuse.fuseRelease = \_ _ -> return ()
  , Fuse.fuseSynchronizeFile = \_ _ -> return Fuse.eNOSYS
  , Fuse.fuseOpenDirectory = fuseOpenDirectory
  , Fuse.fuseReadDirectory = fuseReadDirectory
  , Fuse.fuseReleaseDirectory = \_ -> return Fuse.eNOSYS
  , Fuse.fuseSynchronizeDirectory = \_ _ -> return Fuse.eNOSYS
  , Fuse.fuseAccess = \_ _ -> return Fuse.eNOSYS
  , Fuse.fuseInit = return ()
  , Fuse.fuseDestroy = return ()
  }

fuseGetFileStat :: FilePath -> IO (Either Errno FileStat)
fuseGetFileStat path = do
  case (fromFilePath path) of
    NormalPath n -> posixGetFileStat n
    ZipPath zip rel -> zipGetFileStat zip rel

-- fuseReadSymbolicLink :: FilePath -> IO (Either Errno FilePath)
-- fuseCreateDevice :: FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno
-- fuseCreateDirectory :: FilePath -> FileMode -> IO Errno
-- fuseRemoveLink :: FilePath -> IO Errno
-- fuseRemoveDirectory :: FilePath -> IO Errno
-- fuseCreateSymbolicLink :: FilePath -> FilePath -> IO Errno
-- fuseRename :: FilePath -> FilePath -> IO Errno
-- fuseCreateLink :: FilePath -> FilePath -> IO Errno
-- fuseSetFileMode :: FilePath -> FileMode -> IO Errno
-- fuseSetOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO Errno
-- fuseSetFileSize :: FilePath -> FileOffset -> IO Errno
-- fuseSetFileTimes :: FilePath -> EpochTime -> EpochTime -> IO Errno
fuseOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno FHandle)
fuseOpen _ _ _ = return $ Right ()

fuseRead :: FilePath
         -> FHandle
         -> ByteCount
         -> FileOffset
         -> IO (Either Errno S.ByteString)
fuseRead path fh len off = do
  case (fromFilePath path) of
    NormalPath _ -> return $ Left Fuse.eNOSYS
    ZipPath zip rel -> zipRead zip rel fh len off

-- fuseWrite :: FilePath -> FHandle -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
-- fuseGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
-- fuseFlush :: FilePath -> FHandle -> IO Errno
-- fuseRelease :: FilePath -> FHandle -> IO ()
-- fuseSynchronizeFile :: FilePath -> SyncType -> IO Errno
fuseOpenDirectory :: FilePath -> IO Errno
fuseOpenDirectory path = do
  case (fromFilePath path) of
    NormalPath n -> posixOpenDirectory n
    ZipPath zip rel -> zipOpenDirectory zip rel

fuseReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
fuseReadDirectory path = do
  case (fromFilePath path) of
    NormalPath n -> posixReadDirectory n
    ZipPath zip rel -> zipReadDirectory zip rel

-- fuseReleaseDirectory :: FilePath -> IO Errno
-- fuseSynchronizeDirectory :: FilePath -> SyncType -> IO Errno
-- fuseAccess :: FilePath -> Int -> IO Errno
-- fuseInit :: IO ()
-- fuseDestroy :: IO ()

fromFileStatus :: File.FileStatus -> FileStat
fromFileStatus stat = FileStat
  { Fuse.statEntryType = Fuse.fileModeToEntryType $ File.fileMode stat
  , Fuse.statFileMode = File.fileMode stat
  , Fuse.statLinkCount = File.linkCount stat
  , Fuse.statFileOwner = File.fileOwner stat
  , Fuse.statFileGroup = File.fileGroup stat
  , Fuse.statSpecialDeviceID = File.specialDeviceID stat
  , Fuse.statFileSize = File.fileSize stat
  , Fuse.statBlocks = 1
  , Fuse.statAccessTime = File.accessTime stat
  , Fuse.statModificationTime = File.modificationTime stat
  , Fuse.statStatusChangeTime = File.statusChangeTime stat
  }

regularFile :: FileStat
regularFile = FileStat
  { Fuse.statEntryType = Fuse.RegularFile
  , Fuse.statFileMode = filemode
  , Fuse.statLinkCount = 1
  , Fuse.statFileOwner = 1000
  , Fuse.statFileGroup = 1000
  , Fuse.statSpecialDeviceID = 0
  , Fuse.statFileSize = 0
  , Fuse.statBlocks = 1
  , Fuse.statAccessTime = 0
  , Fuse.statModificationTime = 0
  , Fuse.statStatusChangeTime = 0
  }
    where
      readonly = [File.ownerReadMode, File.groupReadMode, File.otherReadMode]
      filemode = foldr1 unionFileModes readonly

withTimes :: EpochTime -> FileStat -> FileStat
withTimes epoch stat = stat
  { Fuse.statAccessTime = epoch
  , Fuse.statModificationTime = epoch
  , Fuse.statStatusChangeTime = epoch
  }

entryToStat :: Zip.Entry -> FileStat
entryToStat Zip.Entry
  { Zip.eUncompressedSize = filesize
  , Zip.eLastModified = lastmod
  } =
    let epoch = fromIntegral lastmod
        size = fromIntegral filesize
     in withTimes epoch regularFile { Fuse.statFileSize = size }

getZip :: FilePath -> IO Zip.Archive
getZip = fmap Zip.toArchive . B.readFile

filesInZip :: FilePath -> IO [FilePath]
filesInZip path = getZip path >>= return . Zip.filesInArchive

prefixDirs :: FilePath -> [FilePath]
prefixDirs = fmap addTrailingPathSeparator . init . prefixes

allpaths :: FilePath -> [FilePath]
allpaths path = (prefixDirs path) ++ [path]

toFile :: FilePath -> FilePath
toFile path = (if isDirPath path then takeDirectory else id) path

inDir :: FilePath -> FilePath -> Bool
inDir dir path = (== dir) $ takeDirectory $ toFile path

fakeStat :: FilePath -> FilePath -> FileStat -> (FilePath, FileStat)
fakeStat path rel stat =
  (name, transform stat)
    where
      file = toFile path
      name = makeRelative rel file
      transform = if isDirPath path then toDirStat else id

unique :: Eq a => [a] -> [a]
unique = unique' []
  where
    unique' acc [] = acc
    unique' acc (x:xs)
      | elem x acc = unique' acc xs
      | otherwise = unique' (acc ++ [x]) xs

allZipPaths :: Zip.Archive -> [FilePath]
allZipPaths = unique . concatMap allpaths . Zip.filesInArchive

zipGetFileStat :: FilePath -> FilePath -> IO (Either Errno FileStat)
zipGetFileStat zip rel =
  if rel == "."
     then posixStat zip >>= return . Right . toDirStat
     else do
       zipstat <- posixStat zip
       archive <- getZip zip
       let paths = allZipPaths archive
           dirs = filter isDirPath paths
           files = filter (not . flip elem dirs) paths
        in case (elem rel $ map toFile dirs, rel `elem` files) of
             (True, _) -> return $ Right $ toDirStat zipstat
             (_, True) -> do
               case Zip.findEntryByPath rel archive of
                 Just entry -> return $ Right $ entryToStat entry
                 _ -> return $ Left Fuse.eNOENT
             _ -> return $ Left Fuse.eNOENT

zipRead :: FilePath
        -> FilePath
        -> FHandle
        -> ByteCount
        -> FileOffset
        -> IO (Either Errno S.ByteString)
zipRead zip rel _fh len off = do
  getZip zip >>= return . maybe fault extract . findEntry
    where
      findEntry = Zip.findEntryByPath rel
      extract =
        let blen = read $ show $ len
            boff = read $ show $ off
         in Right . B.toStrict . B.take blen . B.drop boff . Zip.fromEntry
      fault = Left Fuse.eFAULT

-- TODO: this should fail if the dir doesn't exist
zipOpenDirectory :: FilePath -> FilePath -> IO Errno
zipOpenDirectory _zip _rel = return Fuse.eOK

zipReadDirectory :: FilePath
                 -> FilePath
                 -> IO (Either Errno [(FilePath, FileStat)])
zipReadDirectory zip rel = do
  zipstat <- posixStat zip
  files <- filesInZip zip
  return $ Right $ filestats zipstat files
    where
      dotDirs :: FileStat -> [(FilePath, FileStat)]
      dotDirs stat = (\name -> (name, toDirStat stat)) <$> [".", ".."]
      filestats :: FileStat -> [FilePath] -> [(FilePath, FileStat)]
      filestats s files = (dotDirs s) ++ (fakefiles s $ relevant files)
      fakefiles :: FileStat -> [FilePath] -> [(FilePath, FileStat)]
      fakefiles stat paths = map (\path -> fakeStat path rel stat) paths
      relevant :: [FilePath] -> [FilePath]
      relevant = filter (inDir rel) . unique . concatMap allpaths

posixStat :: FilePath -> IO FileStat
posixStat path = File.getSymbolicLinkStatus path >>= return . fromFileStatus

nameStat :: FilePath -> FilePath -> IO [(FilePath, FileStat)]
nameStat path name = do
  stat <- posixStat $ Path.combine path name
  return $ [(name,stat)] ++ if isZipFile name
                               then [(toPseudoZipDir name, toDirStat stat)]
                               else []

toDirStat :: FileStat -> FileStat
toDirStat stat = stat
  { Fuse.statEntryType = Fuse.Directory
  , Fuse.statFileMode = foldr1 unionFileModes [ File.ownerReadMode
                                              , File.ownerExecuteMode
                                              , File.groupReadMode
                                              , File.groupExecuteMode
                                              , File.otherReadMode
                                              , File.otherExecuteMode
                                              ]
  , Fuse.statLinkCount = 2
  }

translateErrno :: GIE.IOException -> Errno
translateErrno GIE.IOError { GIE.ioe_errno = Just n } = Errno n
translateErrno _ = Fuse.eNOSYS

posixGetFileStat :: FilePath -> IO (Either Errno FileStat)
posixGetFileStat path = do
  handleIO (return . Left . translateErrno) $ do
    stat <- File.getSymbolicLinkStatus path
    return $ Right $ fromFileStatus stat

posixOpenDirectory :: FilePath -> IO Errno
posixOpenDirectory path = do
  handleIO (return . translateErrno) $ do
    stat <- File.getSymbolicLinkStatus path
    return $ if File.isDirectory stat
                then Fuse.eOK
                else Fuse.eNOENT

posixReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
posixReadDirectory path = do
  handleIO (return . Left . translateErrno) $ do
    names <- Dir.openDirStream path >>= readdir
    statPairs <- forM names $ nameStat path
    return $ Right $ mconcat statPairs
      where
        readdir :: Dir.DirStream -> IO [FilePath]
        readdir = flip entries []
        entries :: Dir.DirStream -> [FilePath] -> IO [FilePath]
        entries s fs = do
          next <- Dir.readDirStream s
          case next of
            "" -> Dir.closeDirStream s >> return fs
            f -> entries s (fs ++ [f])
