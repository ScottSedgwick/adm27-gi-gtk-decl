{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE Rank2Types        #-}
module View where

import           GI.Gtk                        hiding (Bin, (:=), main, on)
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

import           Data.Text                     (Text)
import           Lens.Micro

import           Model                         

mkEditRow :: FromWidget (Bin ListBoxRow) target => Model -> Text -> (Lens' Model Text -> Text -> Event) -> Lens' Model Text -> target Event
mkEditRow m label evt l = bin ListBoxRow [] 
                        $ container Box []
                        [ widget Label [ #label := label,   #widthRequest := 100 ]
                        , widget Entry [ #text := (m ^. l), #widthRequest := 280, onM #changed (fmap (evt l) . entryGetText) ]
                        ]

mkRow :: FromWidget (Bin ListBoxRow) target => Text -> Event -> target Event
mkRow label evt = bin ListBoxRow [] 
                $ container Box []
                [ widget Button [ #label := label, on #clicked evt ]
                ]

view' :: Model -> AppView Window Event
view' st = bin Window [ #title := "ADM.27 Generator"
                      , on #deleteEvent (const (True, Closed))
                      , #widthRequest := 400
                      , #heightRequest := 300
                      ]
  $ case _form st of
      FormMain     -> mainForm st
      FormGuide    -> guideForm st
      FormGuardian -> mainForm st
      FormActivity -> mainForm st
      
mainForm :: FromWidget (Bin ScrolledWindow) target => Model -> target Event
mainForm _ = bin ScrolledWindow [] $ container ListBox []
  [ mkRow "New Guide" NewGuide ]
      
guideForm :: FromWidget (Bin ScrolledWindow) target => Model -> target Event
guideForm m = bin ScrolledWindow [] $ container ListBox []
  [ mkEditRow m "GuideName" ChangeText guideName
  , mkRow "Back To Main" (GotoForm form FormMain) 
  ]
    