{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Database where

import           Database.Selda
import           Database.Selda.SQLite

import           Model    

database :: FilePath
database = "data/adm27.db"

fromGuides :: Table Guide
fromGuides = table "Guides" []

fromGuardians :: Table Guardian
fromGuardians = table "Guardians" []

fromGuideEvents :: Table GuideEvent
fromGuideEvents = table "GuideEvents" []

-- getAll :: (MonadIO m, MonadMask m, SqlRow a, Generic a, Result a) => Table a -> m [a]
getAll fromTable = withSQLite database $ query $ select fromTable

-- withSQLite :: (MonadIO m, MonadMask m) => FilePath -> SeldaT m a -> m a
-- query :: (MonadSelda m, Result a) => Query s a -> m [Res a]
-- select :: Relational a => Table a -> Query s (Row s a)