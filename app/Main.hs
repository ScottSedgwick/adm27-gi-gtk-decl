{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent.Async      (async)
import           Control.Monad                 (void)
import           Data.ByteString               (ByteString)

import qualified GI.Gdk                        as Gdk
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative.App.Simple

import           Database

import           Model                         
import           View                          (view')
import           Update                        (update')

initState :: Model
initState = Model { _form = FormMain 
                  , _currentGuide = emptyGuide
                  , _guides = []
                  }

styles :: ByteString
styles = mconcat
  [ "button { border: 2px solid gray; font-weight: 800; }"
  , ".selected { background: white; border: 2px solid black; }"
  -- Specific color classes:
  , ".red { color: red; }"
  , ".green { color: green; }"
  , ".blue { color: blue; }"
  , ".yellow { color: goldenrod; }"
  ]

main :: IO ()
main = do
  void $ Gtk.init Nothing

  -- Set up screen and CSS provider
  screen <- maybe (fail "No screen?!") return =<< Gdk.screenGetDefault
  p      <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData p styles
  Gtk.styleContextAddProviderForScreen
    screen
    p
    (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)

  -- Load initial data set
  initGuides <- getAll fromGuides 
  -- initGuardians <- withSQLite database $ select fromGuardians
  -- initGuideEvents <- withSQLite database $ select fromGuideEvents
  let app = App {view = view', update = update', inputs = [], initialState = initState { _guides = initGuides } }

  -- Start main loop
  void . async $ do
    void $ runLoop app
    Gtk.mainQuit
  Gtk.main
  