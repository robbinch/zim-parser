-- | Module      : Codec.Archive.Zim.Parser
-- Description : API for parsing ZIM files
-- Copyright   : (c) Robbin C.
-- License     : GPLv3
-- Maintainer  : Robbin C.
-- Stability   : unstable
-- Portability : portable
--
-- This is a library for parsing ZIM (<http://openzim.org>) files. ZIM files
-- contain offline web content (eg, Wikipedia) which can be browsed locally
-- without an Internet connection.
--
-- The high-level functions can be used if it is not a problem to re-open and close
-- the ZIM file on each invocation. For simple browsing on a local device, this
-- should suffice. This also works if the underlying ZIM file is changing.
--
-- The other functions can be used if the caller opts to have more control over
-- resource management.
--
-- Behind the scenes, conduit is used to read from files so memory usage should
-- be constant.
--
-- Below is a full example of a Scotty web server that serves a ZIM file
-- (specified on command line) on localhost port 3000:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Control.Monad.IO.Class (liftIO)
-- > import Data.Text.Lazy (toStrict, fromStrict)
-- > import Data.Text.Encoding (decodeUtf8, encodeUtf8)
-- > import System.Environment (getArgs)
-- > import Network.HTTP.Types.Status (status404)
-- > import Web.Scotty
-- > import Codec.Archive.Zim.Parser (getZimMainPageUrl, getZimUrlContent)
-- >
-- > main :: IO ()
-- > main = do
-- >     [fp] <- getArgs
-- >     scotty 3000 $ do
-- >       get "/" (redirectToZimMainPage fp)
-- >       get (regex "^/(./.*)$") (serveZimUrl fp)
-- >       notFound $ text "Invalid URL!"
-- >
-- > redirectToZimMainPage :: FilePath -> ActionM ()
-- > redirectToZimMainPage fp = do
-- >     res <- liftIO $ getZimMainPageUrl fp
-- >     case res of
-- >       Nothing -> do
-- >         status status404
-- >         text "This ZIM file has no main page specified!"
-- >       Just url -> redirect . fromStrict $ decodeUtf8 url
-- >
-- > serveZimUrl :: FilePath -> ActionM ()
-- > serveZimUrl fp = do
-- >     url <- (encodeUtf8 . toStrict) <$> param "1"
-- >     res <- liftIO $ getZimUrlContent fp url
-- >     case res of
-- >       Nothing -> do
-- >         liftIO . putStrLn $ "Invalid URL: " ++ show url
-- >         status status404
-- >         text $ "Invalid URL!"
-- >       Just (mimeType, content) -> do
-- >         liftIO . putStrLn $ "Serving: " ++ show url
-- >         setHeader "Content-Type" (fromStrict $ decodeUtf8 mimeType)
-- >         raw content
--
-- Feedback and contributions are welcome on <http://github.com/robbinch/zim-parser>.


module Codec.Archive.Zim.Parser
       (
       -- * High-level Functions
       -- | The following high-level functions are sufficient to program a simple webserver that serves ZIM files (see example above).
         getZimMainPageUrl
       , getZimUrlContent
       -- * Searching
       , searchZimDirEntByUrl
       , searchZimDirEntByTitle
       , searchZimDirEntByTitlePrefix
       -- * Exceptions
       , ZimException(..)
       -- * ZIM Header
       , ZimHeader(..)
       , getZimHeader
       , getZimMimeList
       -- * ZIM Directory Entry
       , ZimDirEntType(..)
       , ZimDirEnt(..)
       , getZimDirEntByUrlIndex
       , getZimDirEntByTitleIndex
       -- * ZIM Content
       , getZimCluster
       , getZimBlob
       , getZimContentByUrlIndex
       -- * ZIM file format
       -- | Following is a short summary of the ZIM file format.
       -- The authoritative reference is at http://www.openzim.org/wiki/ZIM_file_format.
       --
       -- === 1. ZIM header
       -- This is an 80-byte header (see 'ZimHeader'). Among other things, it contains file offsets to the below.
       --
       -- === 2. List of MIME types
       -- This is a sequence of null-terminated strings (eg. @text\/html@, @text\/javascript@). The last string is zero length, so
       -- the end always consists of 2 consecutive null bytes.
       --
       -- === 3. List of URLs
       -- This is a sequence of 8-byte file offsets, each pointing to a directory entry. This list is sorted by the directory entries' URL.
       --
       -- 'getZimDirEntByUrlIndex' looks up this table to return a directory entry.
       --
       -- === 4. List of Titles
       -- This is a sequence of 4-byte indices, each pointing to a URL above (which in turn point to a directory entry).
       -- This list is sorted by the directory entries' Title.
       --
       -- 'getZimDirEntByTitleIndex' uses this table to return a directory entry.
       --
       -- === 5. Directory Entries
       -- This is a sequence of Directory Entries (see 'ZimDirEnt').
       -- The first 2 bytes determine the type of this entry, which also determine the length.
       -- Contents include:
       --
       -- ==== a. MIME type
       -- This 2-byte field means:
       --
       -- [@0xffff@] This directory entry is a 'ZimRedirectEntry'.
       -- [@0xfffe@] This directory entry is a 'ZimLinkTarget'.
       -- [@0xfffd@] This directory entry is a 'ZimDeletedEntry'.
       -- [@any other value@] This directory entry is a 'ZimArticleEntry' and this index into the MIME list from above determines its MIME type.
       --
       -- ==== b. Namespace
       -- This single character determines the directory entry's namespace. (eg. __A__ for articles, __I__ for images, etc.)
       -- The comprehensive list is at http://www.openzim.org/wiki/ZIM_file_format#Namespaces.
       --
       -- ==== c. Cluster and Blob number
       -- Only for 'ZimArticleEntry', this is the directory entry's Cluster and Blob number.
       -- The Cluster number is a 4-byte index into the list of Clusters below.
       -- The Blob number refers to a block inside the (decompressed) cluster.
       -- Together, they provide the content of this directory entry.
       --
       -- ==== d. URL and Title
       -- These 2 null-terminated strings represent the URL and Title of this directory entry respectively.
       -- If the Title is empty, it is taken to be the same as the URL.
       --
       -- === 6. List of Clusters
       -- This is a list of 8-byte file offsets, each pointing to a cluster in the file.
       -- The end of a cluster is also the start of the next cluster.
       -- Therefore, the length of a cluster is the difference between the adjacent offsets.
       -- For the last cluster, the end is the Checksum file offset, as the Checksum is always
       -- the last 16 bytes of a ZIM file.
       --
       -- ==== a. Compression Type
       -- The first byte of the cluster determines if it is uncompressed (eg. PNG image) or compressed with LZMA (eg. HTML).
       --
       -- [@0 or 1@] No compression
       -- [@4@] Compressed with LZMA
       --
       -- ==== b. List of Blobs
       -- This is a list of 4-byte offsets, each pointing inside this cluster.
       -- The end of a blob is also the start of the next blob.
       -- Therefore, the length of a blob is the difference between the adjacent offsets.
       -- The last offset points to the end of the data area so there is always one more offset than blobs.
       ) where

import Control.Exception (Exception, throw)
import Control.Monad (when)
import Data.Char (chr, ord)
import Data.Maybe (catMaybes, fromMaybe, fromJust)
import Data.Typeable (Typeable)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import System.IO (Handle, IOMode(ReadMode), withBinaryFile, openBinaryFile)

import Data.Conduit (($$), (=$), await, Sink)
import Data.Conduit.Binary (sourceHandleRange, sourceLbs)
import Data.Conduit.Serialization.Binary (sinkGet, conduitGet)

import Data.Array.IArray ((!), listArray, Array)
import Codec.Compression.Lzma (decompress)
import Data.Binary.Get (Get, skip, getWord8, getWord16le, getWord32le, getWord64le, getByteString, getLazyByteStringNul, getRemainingLazyByteString)
import Numeric (showHex)

-- | Other than the below, ErrorCall can be thrown by LZMA library if there is a problem with decompression.
data ZimException = ZimInvalidMagic       -- ^ ZIM file has invalid magic number (anything other than 72173914).
                  | ZimParseError String  -- ^ There is an error in parsing.
                  | ZimIncompleteInput    -- ^ There is insufficient bytes required to parse.
                  | ZimInvalidIndex Int   -- ^ The given index (URL, title or cluster) is out of bounds for this ZIM file.
                  deriving (Show, Typeable)
instance Exception ZimException

-- | See http://www.openzim.org/wiki/ZIM_file_format#Header for more details.
data ZimHeader = ZimHeader
    {
    -- | Magic Number of file (somewhat superfluous as 'getZimHeader' will throw an exception if magic number is anything other than 72173914)
      zimMagicNumber   :: Int
    -- | Version of ZIM header
    , zimVersion       :: Int
    -- | UUID of file
    , zimUuid          :: B.ByteString
    -- | Number of articles
    , zimArticleCount  :: Int
    -- | Number of clusters
    , zimClusterCount  :: Int
    -- | Position of sorted URL pointers
    , zimUrlPtrPos     :: Integer
    -- | Position of sorted Title pointers
    , zimTitlePtrPos   :: Integer
    -- | Position of Cluster pointers
    , zimClusterPtrPos :: Integer
    -- | Position of MIME list
    , zimMimeListPos   :: Integer
    -- | Index of main page
    , zimMainPage      :: Maybe Int
    -- | Index of layout page
    , zimLayoutPage    :: Maybe Int
    -- | Position of MD5 checksum
    , zimChecksumPos   :: Integer
    } deriving (Show, Eq)

-- | There are 4 types of directory entries. Most content in a ZIM file are
-- usually 'ZimArticleEntry' or 'ZimRedirectEntry'.
data ZimDirEntType = ZimArticleEntry
                   | ZimRedirectEntry
                   | ZimLinkTarget
                   | ZimDeletedEntry
                   deriving (Eq, Show)

-- | See http://www.openzim.org/wiki/ZIM_file_format#Directory_Entries for more details.
data ZimDirEnt = ZimDirEnt
    {
    -- | Type of this Directory Entry
      zimDeType          :: ZimDirEntType
    -- | Index into MIME list given by 'getZimMimeList'
    , zimDeMimeType      :: Int
    -- | Parameter Length
    , zimDeParameterLen  :: Int
    -- | Namespace
    , zimDeNamespace     :: Char
    -- | Revision
    , zimDeRevision      :: Int
    -- | Redirect Index (only applicable for 'ZimRedirectEntry')
    , zimDeRedirectIndex :: Maybe Int
    -- | Content is stored in this Cluster Number (only applicable for 'ZimArticleEntry')
    , zimDeClusterNumber :: Maybe Int
    -- | Content is stored in this Blob Number (only applicable for 'ZimArticleEntry')
    , zimDeBlobNumber    :: Maybe Int
    -- | URL
    , zimDeUrl           :: B8.ByteString
    -- | Title
    , zimDeTitle         :: B8.ByteString
    -- , zimDeParameter     :: BL.ByteString -- unused
    } deriving (Eq, Show)

-- | Parses ZIM Header from a file handle.
-- A ZIM Header is used by most of the functions in this module.
-- For better performance or resource management, multiple file handles
-- can be opened with the same ZIM header in order to call
-- functions in parallel.
-- If the underlying ZIM file has changed, a new ZIM header should be parsed.
getZimHeader :: Handle        -- ^ Handle to ZIM file (eg. previously returned from 'openBinaryFile' or 'withBinaryFile')
             -> IO ZimHeader  -- ^ Returns ZIM Header
getZimHeader hdl = src $$ sinkGet parseZimHeader
  where (pos, len) = (Just 0, Just 80)
        src        = sourceHandleRange hdl pos len

parseZimHeader :: Get ZimHeader
parseZimHeader = do
    magicNumber   <- fromIntegral <$> getWord32le
    when (magicNumber /= 72173914) $ throw ZimInvalidMagic
    version       <- fromIntegral <$> getWord32le
    uuid          <- getByteString 16
    articleCount  <- fromIntegral <$> getWord32le
    clusterCount  <- fromIntegral <$> getWord32le
    urlPtrPos     <- fromIntegral <$> getWord64le
    titlePtrPos   <- fromIntegral <$> getWord64le
    clusterPtrPos <- fromIntegral <$> getWord64le
    mimeListPos   <- fromIntegral <$> getWord64le
    mainPage      <- fromIntegral <$> getWord32le
    layoutPage    <- fromIntegral <$> getWord32le
    checksumPos   <- fromIntegral <$> getWord64le
    return $ ZimHeader magicNumber version uuid articleCount clusterCount
                       urlPtrPos titlePtrPos clusterPtrPos mimeListPos
                       (if mainPage == 0xffffffff then Nothing else Just mainPage)
                       (if layoutPage == 0xffffffff then Nothing else Just layoutPage)
                       checksumPos

-- | Parses MIME List from a ZIM header and a file handle.
getZimMimeList :: ZimHeader                     -- ^ ZIM header
               -> Handle                        -- ^ Handle to ZIM file
               -> IO (Array Int B8.ByteString)  -- ^ Returns array of MIME types
getZimMimeList hdr hdl = do
    mimeList <- src $$ parseByteStringsNul
    return $ listArray (0, length mimeList) mimeList
  where (pos, len) = (Just $ zimMimeListPos hdr, Nothing)
        src        = sourceHandleRange hdl pos len

-- Parses a list of null-terminated byte sequence.
-- Last entry is zero length (end of block is always 2 null bytes).
parseByteStringsNul :: Sink B8.ByteString IO [B8.ByteString]
parseByteStringsNul = conduitGet getLazyByteStringNul =$ loop id
  where loop :: ([B8.ByteString] -> [B8.ByteString]) -> Sink BL.ByteString IO [B8.ByteString]
        loop front = await >>= maybe
            (return $ front [])
            (\x -> let bs = BL.toStrict x
                   in if B8.null bs then return (front []) else loop (front . (bs:))
            )

-- | Returns Directory Entry corresponding to URL index.
getZimDirEntByUrlIndex :: ZimHeader     -- ^ ZIM header
                       -> Handle        -- ^ Handle to ZIM file
                       -> Int           -- ^ URL index
                       -> IO ZimDirEnt  -- ^ Returns a Directory Entry
getZimDirEntByUrlIndex hdr hdl i = do
    when (i < 0 || i >= zimArticleCount hdr) . throw $ ZimInvalidIndex i
    dePos <- sourceHandleRange hdl urlPtrPos Nothing $$ sinkGet getWord64le
    sourceHandleRange hdl (Just $ fromIntegral dePos) Nothing $$ sinkGet parseZimDirEnt
  where urlPtrPos = Just $ zimUrlPtrPos hdr + 8 * fromIntegral i

parseZimDirEnt :: Get ZimDirEnt
parseZimDirEnt = do
    mimeType  <- fromIntegral       <$> getWord16le :: Get Int
    parmLen   <- fromIntegral       <$> getWord8
    namespace <- chr . fromIntegral <$> getWord8
    revision  <- fromIntegral       <$> getWord32le

    -- parsing of next 3 fields depends on mimeType
    let deType = case mimeType of
          0xffff -> ZimRedirectEntry
          0xfffe -> ZimLinkTarget
          0xfffd -> ZimDeletedEntry
          _      -> ZimArticleEntry
    (redirectIndex, clusterNumber, blobNumber ) <-
      case deType of
        ZimArticleEntry  ->
          (\x y -> (Nothing, Just $ fromIntegral x, Just $ fromIntegral y))
            <$> getWord32le <*> getWord32le
        ZimRedirectEntry ->
          (\x -> (Just $ fromIntegral x, Nothing, Nothing))
            <$> getWord32le
        ZimLinkTarget    -> skip 8 >> return (Nothing, Nothing, Nothing)
        ZimDeletedEntry  -> skip 8 >> return (Nothing, Nothing, Nothing)
        :: Get (Maybe Int, Maybe Int, Maybe Int)

    url   <- BL.toStrict <$> getLazyByteStringNul
    title <- BL.toStrict <$> getLazyByteStringNul
    return $ ZimDirEnt deType mimeType parmLen namespace revision redirectIndex
                       clusterNumber blobNumber url
                       -- specs: title is same as url if title is empty
                       (if B.null title then url else title)

-- | Returns Directory Entry corresponding to Title index.
getZimDirEntByTitleIndex :: ZimHeader     -- ^ ZIM header
                         -> Handle        -- ^ Handle to ZIM file
                         -> Int           -- ^ Title index
                         -> IO ZimDirEnt  -- ^ Returns a Directory Entry
getZimDirEntByTitleIndex hdr hdl i = do
    when (i < 0 || i >= zimArticleCount hdr) . throw $ ZimInvalidIndex i
    urlIndex <- sourceHandleRange hdl titlePtrPos Nothing $$ sinkGet getWord32le
    getZimDirEntByUrlIndex hdr hdl (fromIntegral urlIndex)
  where titlePtrPos = Just $ zimTitlePtrPos hdr + 4 * fromIntegral i

-- | Returns (decompressed) Cluster corresponding to Cluster number.
-- This can throw ErrorCall if there is an error during decompression.
getZimCluster :: ZimHeader         -- ^ ZIM header
              -> Handle            -- ^ Handle to ZIM file
              -> Int               -- ^ Cluster number
              -> IO BL.ByteString  -- ^ Returns a lazy bytestring containing cluster
getZimCluster hdr hdl i = do
    let limit = zimClusterCount hdr - 1
    when (i < 0 || i > limit) . throw $ ZimInvalidIndex i
    let clusterPos = Just $ zimClusterPtrPos hdr + 8 * fromIntegral i
    (pos0, pos1) <- sourceHandleRange hdl clusterPos Nothing $$
                      sinkGet $ (,) <$> getWord64le <*> getWord64le
    -- length of last cluster is determined by checksum pos instead of next cluster pos
    let len = if i == limit
                  then (fromIntegral $ zimChecksumPos hdr) - pos0
                  else pos1 - pos0
        toI = Just . fromIntegral
    bs <- sourceHandleRange hdl (toI pos0) (toI len) $$
            sinkGet getRemainingLazyByteString

    case BL.uncons bs of
      Just (0, cluster) -> return cluster
      Just (1, cluster) -> return cluster
      Just (4, cluster) -> return $ decompress cluster
      Just (x, _)       -> throw . ZimParseError $
        "Cluster " ++ show i ++
        " (offset: " ++ showHex pos0 "" ++ ", length: " ++ show len ++
        ") compressed with unsupported type: " ++ show x
      Nothing           -> throw . ZimParseError $
        "Insufficient bytes for cluster " ++ show i

-- | Returns Blob given Cluster and Blob number.
getZimBlob :: ZimHeader         -- ^ ZIM header
           -> Handle            -- ^ Handle to ZIM file
           -> Int               -- ^ Cluster Number
           -> Int               -- ^ Blob Number
           -> IO BL.ByteString  -- ^ Returns a lazy bytestring containing blob
getZimBlob hdr hdl c b = do
    cluster      <- getZimCluster hdr hdl c
    (pos0, pos1) <- sourceLbs (BL.drop (4 * fromIntegral b) cluster) $$
                      sinkGet $ (,) <$> getWord32le <*> getWord32le
    let len = pos1 - pos0
    return . BL.take (fromIntegral len) $ BL.drop (fromIntegral pos0) cluster

-- | Returns content given URL index. Redirects are handled automatically
getZimContentByUrlIndex :: ZimHeader         -- ^ ZIM header
                        -> Handle            -- ^ Handle to ZIM file
                        -> Int               -- ^ URL index
                        -> IO BL.ByteString  -- ^ Returns a lazy bytestring containing content
getZimContentByUrlIndex hdr hdl i = do
    de <- getZimDirEntByUrlIndex hdr hdl i
    case zimDeType de of
      ZimRedirectEntry -> getZimContentByUrlIndex hdr hdl (fromJust $ zimDeRedirectIndex de)
      ZimArticleEntry  -> let (Just c, Just b) = (zimDeClusterNumber de, zimDeBlobNumber de)
                          in getZimBlob hdr hdl c b
      _                -> return BL.empty

-- Binary Search implementation used for searching sorted URL and Title lists.
binarySearch :: (Int -> IO (Ordering, a)) -> Int -> Int -> IO (Maybe a)
binarySearch f low high =
    if high < low
        then return Nothing
        else do
            let mid = (low + high) `div` 2
            (o, x) <- f mid
            case o of
              LT -> binarySearch f low       (mid -1)
              GT -> binarySearch f (mid + 1) high
              EQ -> return $ Just x

-- | Search for a Directory Entry given a URL.
-- URL must be prefixed with Namespace (eg. "A\/Blue.html" or "I\/favicon.png").
searchZimDirEntByUrl :: ZimHeader                    -- ^ ZIM header
                     -> Handle                       -- ^ Handle to ZIM file
                     -> B.ByteString                 -- ^ URL to search for
                     -> IO (Maybe (Int, ZimDirEnt))  -- ^ Returns (URL Index, Directory Entry) if found.
searchZimDirEntByUrl hdr hdl url =
    binarySearch f 0 (zimArticleCount hdr - 1)
  -- prepend namespace when comparing URLs
  where f i = do
          de <- getZimDirEntByUrlIndex hdr hdl i
          let v = zimDeNamespace de `B8.cons` '/' `B8.cons` zimDeUrl de
          return (compare url v, (i, de))

-- | Search for a Directory Entry given a Title and namespace.
searchZimDirEntByTitle :: ZimHeader                   -- ^ ZIM header
                       -> Handle                      -- ^ Handle to ZIM file
                       -> Char                        -- ^ Namespace to search for
                       -> B.ByteString                -- ^ Title to search for
                       -> IO (Maybe (Int, ZimDirEnt)) -- ^ Returns (Title Index, Directory Entry) if found
searchZimDirEntByTitle hdr hdl n title =
    binarySearch f 0 (zimArticleCount hdr - 1)
  where title' = n `B8.cons` '/' `B8.cons` title
        f i = do
          de <- getZimDirEntByTitleIndex hdr hdl i
          let v = zimDeNamespace de `B8.cons` '/' `B8.cons` zimDeTitle de
          return (compare title' v, (i, de))

-- | Search for lower and upper bounds of Title indices that contains prefix in their title.
-- Eg, if title list comprises \[ \"A\", \"Ba\", \"Bb\", \"Bc\", \"C\" \] prefix
-- search for \"B\" will return bounds corresponding to (\"Ba\", \"Bc\").
searchZimDirEntByTitlePrefix :: ZimHeader     -- ^ ZIM header
                             -> Handle        -- ^ Handle to ZIM file
                             -> Char          -- ^ Namespace
                             -> B.ByteString  -- ^ Title Prefix
                             -> IO (Maybe ((Int, ZimDirEnt), (Int, ZimDirEnt)))  -- ^ Returns ((Lower Title Index, Lower Directory Entry), (Upper Title Index, Upper Directory Entry)) if found.
searchZimDirEntByTitlePrefix hdr hdl n pre = do
    lb <- binarySearch lowerBound 0 limit
    case lb of
      Nothing -> return Nothing
      _ -> do
        ub <- binarySearch upperBound 0 limit
        return . Just $ (fromJust lb, fromJust ub)
  where pre' = n `B8.cons` '/' `B8.cons` pre
        preLen = B8.length pre
        limit = zimArticleCount hdr - 1
        -- extracts title to compare from Directory Entry
        mkT x = zimDeNamespace x `B8.cons` '/' `B8.cons` B8.take preLen (zimDeTitle x)
        g idx = (\x -> (x, mkT x)) <$> getZimDirEntByTitleIndex hdr hdl idx
        -- i has to be the entry just before prefix matches
        lowerBound i = do
          de <- getZimDirEntByTitleIndex hdr hdl i
          case compare pre' (mkT de) of
            -- if prefix matches, we still return LT as we want to find the entry BEFORE.
            -- special case: if i = 0, then this is the lower bound.
            EQ  -> if i == 0
                      then return (EQ, (i, de))
                      else return (LT, (i, de))
            lgt -> do
              -- if succeeding entry has prefix, that is the lower bound.
              (de', v') <- g (i + 1)
              if pre' `B8.isPrefixOf` v'
                 then return (EQ, (i + 1, de'))
                 else return (lgt, (i, de))
        upperBound i = do
          de <- getZimDirEntByTitleIndex hdr hdl i
          case compare pre' (mkT de) of
            EQ  -> if i == limit
                      then return (EQ, (i, de))
                      else return (GT, (i, de))
            lgt -> do
              (de', v') <- g (i - 1)
              if pre' `B8.isPrefixOf` v'
                 then return (EQ, (i - 1, de'))
                 else return (lgt, (i, de))

-- | Returns URL of main page in ZIM.
-- This URL can be used for redirecting to the actual page.
getZimMainPageUrl :: FilePath                 -- ^ Path to ZIM file
                  -> IO (Maybe B.ByteString)  -- ^ Returns URL if found
getZimMainPageUrl fp = do
    withBinaryFile fp ReadMode $ \hdl -> do
        hdr <- getZimHeader hdl
        case zimMainPage hdr of
            Nothing -> return Nothing
            Just i  -> do
              de <- getZimDirEntByUrlIndex hdr hdl i
              return . Just $ zimDeNamespace de `B8.cons` '/' `B8.cons` zimDeUrl de

-- | Returns (MIME type, content) of URL, ready to be served via HTTP.
-- Note that MIME type is a strict bytestring while Content is lazy.
getZimUrlContent :: FilePath                                  -- ^ Path to ZIM file
                 -> B.ByteString                              -- ^ URL
                 -> IO (Maybe (B.ByteString, BL.ByteString))  -- ^ Returns (MIME type, content) if found
getZimUrlContent fp url = do
    withBinaryFile fp ReadMode $ \hdl -> do
        hdr <- getZimHeader hdl
        res <- searchZimDirEntByUrl hdr hdl url
        case res of
            Nothing -> return Nothing
            Just (i, de) -> do
              mimeList <- getZimMimeList hdr hdl
              content  <- getZimContentByUrlIndex hdr hdl i
              return $ Just (mimeList ! zimDeMimeType de, content)
