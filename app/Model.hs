{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Model where

import qualified Data.IntMap                   as M
import           Data.Text                     (Text)
import           Data.Time.Calendar            (Day)
import           Lens.Micro.TH                 (makeLenses)
import           Lens.Micro

import           Adm27.Model

data Form = FormMain
          | FormGuide
          | FormGuardian
          | FormGuideEvent
          deriving (Show, Eq)

data Model = Model
  { _form :: Form
  , _currentGuide :: Guide
  , _guides    :: M.IntMap Guide
  , _currentGuardian :: Guardian
  , _guardians :: M.IntMap Guardian
  , _currentGuideEvent :: GuideEvent
  , _events    :: M.IntMap GuideEvent
  } deriving (Show, Eq)
makeLenses ''Model

data Event = IncrAll
            | GotoForm   Form
            | ChangeText (Lens' Model Text) Text
            | ChangeDate (Lens' Model Day)  Day
            | ChangeBool (Lens' Model Bool) Bool
            | NewGuide
            | NewGuardian
            | NewGuideEvent
            | CommitGuide
            | Closed
