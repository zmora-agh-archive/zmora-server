{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Submit where

import Control.Monad
import Crypto.Hash.SHA1
import Data.Time.Clock as T
import Database.Esqueleto
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Servant.Multipart
import Text.Printf (printf)
import Data.Maybe (mapMaybe)

import Utils.Controller
import Utils.ExtensibleRecords

import Models
import Models.SubmitStatus

instance HasController (CurrentUser -> Key Contest -> Key ContestProblem -> HandlerT IO [Entity Submit]) where
  resourceController user _ problemId = runQuery q
    where q = select $ from $ \submits -> do
            where_ $ submits ^. SubmitProblem ==. val problemId
            where_ $ submits ^. SubmitAuthor ==. val (entityKey user)
            orderBy [ desc (submits ^. SubmitDate)]
            return submits

instance HasController (CurrentUser -> Key Contest -> Key ContestProblem -> Key Submit -> HandlerT IO (Entity Submit)) where
  resourceController user _ _ submitId = runQuery q
    where q = selectOne $ from $ \submits -> do
            where_ $ submits ^. SubmitId ==. val submitId
            where_ $ submits ^. SubmitAuthor ==. val (entityKey user)
            return submits

instance HasController (CurrentUser -> Key Contest -> Key ContestProblem -> Key Submit -> HandlerT IO (Entity' Submit SubmitWithFilesAndTests)) where
  resourceController user _ _ submitId = do
    res <- runQuery $ select $ from $ \(submits `LeftOuterJoin` testResults `InnerJoin` files) -> do
            on      $ submits ^. SubmitId ==. files ^. SubmitFileSubmit
            on      $ just (submits ^. SubmitId) ==. testResults ?. TestResultSubmit
            where_  $ submits ^. SubmitId ==. val submitId
            where_  $ submits ^. SubmitAuthor ==. val (entityKey user)
            return (submits, files, testResults)

    (submit, files) <- safeHead ErrNotFound $ collectionJoin $ map (\(submit, file, _) -> (submit, file)) res
    let tests = mapMaybe (\(_, _, tests) -> tests) res

    return $ Entity' (entityKey submit)
            $ SubmitWithFilesAndTests
            $ rAdd (Var :: Var "files") (fmap truncateFile files)
            $ rAdd (Var :: Var "tests") (fmap truncateTest tests)
            $ explode (entityVal submit)
    where truncateFile file = Entity' (entityKey file)
            $ rDel (Var :: Var "file")
            $ rDel (Var :: Var "submit")
            $ explode $ entityVal file
          truncateTest test = Entity' (entityKey test)
            $ rDel (Var :: Var "submit")
            $ explode $ entityVal test


instance HasController (CurrentUser -> Key Contest -> Key ContestProblem -> MultipartData -> HandlerT IO (Entity' Submit SubmitWithFiles)) where
  resourceController user _ problem formData = do
    currentTimestamp <- liftIO T.getCurrentTime
    let submit = Submit problem (entityKey user) currentTimestamp QUE

    submitFiles <- forM (files formData) $ \fd -> do
      content <- liftIO $ BS.readFile (fdFilePath fd)
      let sha1sum = T.pack $ BS.unpack (hash content) >>= printf "%02x"
      return $ \sid -> do
        let sfile = SubmitFile sid content sha1sum (fdFileName fd)
        fileId <- insert sfile
        return $ Entity' fileId $ rDel (Var :: Var "file") $ explode sfile

    -- TODO Make sure this happens single transaction
    runQuery $ do
      submitId <- insert submit
      files <- sequence $ submitFiles <*> pure submitId
      return $ Entity' submitId $ SubmitWithFiles
                                $ rAdd (Var :: Var "files") files
                                $ explode submit
