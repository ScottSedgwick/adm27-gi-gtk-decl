{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
module Adm27.Database where

import           Data.Hashable                 (hash)
import qualified Data.IntMap                   as M
import           Database.Selda
import           Database.Selda.SQLite

import           Adm27.Model 

-- | Database filename
database :: FilePath
database = "data/adm27.db"

-- | Database structure
tblGuides :: Table Guide
tblGuides = table "Guides" [#_guide_id :- primary]

tblGuardians :: Table Guardian
tblGuardians = table "Guardians" [#_guardian_id :- primary]

tblGuideEvents :: Table GuideEvent
tblGuideEvents = table "GuideEvents" [#_event_id :- primary]

-- | Database initialisation and reading
initDb :: IO (M.IntMap Guide, M.IntMap Guardian, M.IntMap GuideEvent)
initDb = withSQLite database $ do
  tryCreateTable tblGuides
  tryCreateTable tblGuardians
  tryCreateTable tblGuideEvents

  qryGuides    <- query $ select tblGuides
  qryGuardians <- query $ select tblGuardians
  qryEvents    <- query $ select tblGuideEvents

  let initGuides    = M.fromList $ map (\x -> (hash x, x)) qryGuides
  let initGuardians = M.fromList $ map (\x -> (hash x, x)) qryGuardians
  let initEvents    = M.fromList $ map (\x -> (hash x, x)) qryEvents

  return (initGuides, initGuardians, initEvents)
