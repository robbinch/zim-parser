{-# LANGUAGE OverloadedStrings #-}
module Codec.Archive.Zim.ParserSpec (main, spec) where

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust, isJust)
import System.IO (Handle, openBinaryFile, IOMode(ReadMode))

import Data.Array.IArray (bounds, (!))

import Test.Hspec

import Codec.Archive.Zim.Parser

testSmallFilePath :: FilePath
testSmallFilePath = "test/wikipedia_en_ray_charles_2015-06.zim"

main :: IO ()
main = hspec spec

setup :: IO (ZimHeader, Handle)
setup = do
    small <- openBinaryFile testSmallFilePath ReadMode
    smallZim <- getZimHeader small
    return (smallZim, small)

spec :: Spec
spec = do
    describe "zim" $ do
      beforeAll setup $ do

        it "can open Ray Charles ZIM" $ \(smallZim, _) -> do
            zimMagicNumber smallZim `shouldBe` 72173914

        it "can get version" $ \(smallZim, _) -> do
            zimVersion smallZim `shouldBe` 5

        it "can get article count" $ \(smallZim, _) -> do
            zimArticleCount smallZim `shouldBe` 458

        it "can get cluster count" $ \(smallZim, _) -> do
            zimClusterCount smallZim `shouldBe` 215

        it "can parse MIME list" $ \(smallZim, small) -> do
            mimeList <- getZimMimeList smallZim small
            mimeList ! 0 `shouldBe` "application/javascript"
            bounds mimeList `shouldBe` (0, 9)

        it "can get main page" $ \(smallZim, _) -> do
            zimMainPage smallZim `shouldBe` Just 238

        it "can get main page URL" $ \_ -> do
            url <- getZimMainPageUrl testSmallFilePath
            url `shouldBe` Just "A/index.htm"

        it "can lookup directory entry by URL index" $ \(smallZim, small) -> do
            de <- getZimDirEntByUrlIndex smallZim small 0
            zimDeType de `shouldBe` ZimRedirectEntry
            zimDeUrl de `shouldBe` "favicon"

        it "can lookup directory entry by Title index" $ \(smallZim, small) -> do
            de <- getZimDirEntByTitleIndex smallZim small 3
            zimDeTitle de `shouldBe` "(The Night Time Is) The Right Time"
            zimDeNamespace de `shouldBe` 'A'

        it "can lookup cluster 0" $ \(smallZim, small) -> do
            cluster <- getZimCluster smallZim small 0
            BL.length  cluster `shouldBe` 2160083

        it "can get cluster 2 blob 0" $ \(smallZim, small) -> do
            blob <- getZimBlob smallZim small 2 0
            BL.take 4 blob `shouldBe` "\137PNG"

        it "can get content for article 11" $ \(smallZim, small) -> do
            bs <- getZimContentByUrlIndex smallZim small 11
            BL.take 12 bs `shouldBe` "<html><head>"

        it "can search for title" $ \(smallZim, small) -> do
            let title = B8.pack "(The Night Time Is) The Right Time"
            res <- searchZimDirEntByTitle smallZim small 'A' title
            (title, res) `shouldSatisfy` (isJust . snd)
            (fst . fromJust $ res) `shouldBe` 3

        it "can search for non-existent title" $ \(smallZim, small) -> do
            let title = B8.pack "nonexistent-title"
            res <- searchZimDirEntByTitle smallZim small 'A' title
            (title, res) `shouldBe` (title, Nothing)

        it "can search for title prefix" $ \(smallZim, small) -> do
            let title = B8.pack "Blue"
            res <- searchZimDirEntByTitlePrefix smallZim small 'A' title
            (title, res) `shouldSatisfy` (isJust . snd)
            let Just ((lb, _), (ub, _)) = res
            (lb, ub) `shouldBe` (22, 25)

        it "can search for non-existent title prefix" $ \(smallZim, small) -> do
            let title = B8.pack "ZZZ"
            res <- searchZimDirEntByTitlePrefix smallZim small 'A' title
            (title, res) `shouldBe` (title, Nothing)

        it "can search for first title prefix" $ \(smallZim, small) -> do
            let title = B8.pack "fav"
            res <- searchZimDirEntByTitlePrefix smallZim small '-' title
            (title, res) `shouldSatisfy` (isJust . snd)
            let Just ((lb, _), (ub, _)) = res
            (lb, ub) `shouldBe` (0, 0)

        it "can search for last title prefix" $ \(smallZim, small) -> do
            let title = B8.pack "Title"
            res <- searchZimDirEntByTitlePrefix smallZim small 'M' title
            (title, res) `shouldSatisfy` (isJust . snd)
            let Just ((lb, _), (ub, _)) = res
            (lb, ub) `shouldBe` (457, 457)

        it "can search for URLs" $ \(smallZim, small) -> do
            let url = "A/A_Fool_for_You.html"
            res <- searchZimDirEntByUrl smallZim small url
            (url, res) `shouldSatisfy` (isJust . snd)
            (fst . fromJust $ res) `shouldBe` 4

        it "can search for non-existent URL" $ \(smallZim, small) -> do
            let url = "a/zzz.html"
            res <- searchZimDirEntByUrl smallZim small url
            res `shouldBe` Nothing

        it "can get (mimetype, content)" $ \_ -> do
            res <- getZimUrlContent testSmallFilePath "A/index.htm"
            res `shouldSatisfy` isJust
            let Just (m, c) = res
            (m, BL.length c) `shouldBe` ("text/html", 8637)

        let searchAllUrls hdr hdl des =
              forM_ des $ \de -> do
                let ns = zimDeNamespace de
                    url = zimDeUrl de
                    url' = ns `B8.cons` '/' `B8.cons` url
                res <- searchZimDirEntByUrl hdr hdl url'
                (url, res) `shouldSatisfy` (isJust . snd)
                (zimDeUrl . snd . fromJust $ res) `shouldBe` url
            getAllArticles hdr hdl n = do
              let ns = [0 .. n - 1]
              forM_ ns $ \i -> do
                de <- getZimDirEntByUrlIndex hdr hdl i
                let url = zimDeUrl de
                bs <- getZimContentByUrlIndex hdr hdl i
                (url, bs) `shouldSatisfy` (\(_, b) -> BL.length b > 0)

        it "can search all URLs" $ \(smallZim, small) -> do
          deList <- mapM (getZimDirEntByUrlIndex smallZim small) [0 .. zimArticleCount smallZim - 1]
          searchAllUrls smallZim small deList

        it "can get content from all URLs" $ \(smallZim, small) -> do
          getAllArticles smallZim small 457


