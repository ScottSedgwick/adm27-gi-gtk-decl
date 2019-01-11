{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types        #-}
module Model where

import           Data.Text                     (Text)
import           Lens.Micro.TH                 (makeLenses)
import           Lens.Micro

data Form = FormMain
          | FormGuide
          | FormGuardian
          | FormActivity
          deriving (Show, Eq)

data Model = Model
  { _form :: Form
  , _guideName :: Text
  } deriving (Show, Eq)

makeLenses ''Model

data Event = IncrAll
           | GotoForm   (Lens' Model Form) Form
           | ChangeText (Lens' Model Text) Text
           | NewGuide
           | Closed
