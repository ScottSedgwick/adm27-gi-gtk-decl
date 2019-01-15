{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent.Async      (async)
import           Control.Monad                 (void)
import           Data.ByteString               (ByteString)
import qualified Data.IntMap                   as M

import qualified GI.Gdk                        as Gdk
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative.App.Simple

import           Adm27.Model
import           Adm27.Database

import           Model                         
import           View                          (view')
import           Update                        (update')

initState :: Model
initState = Model { _form = FormMain 
                  , _currentGuide = emptyGuide
                  , _guides = M.empty
                  , _currentGuardian = emptyGuardian
                  , _guardians = M.empty
                  , _currentGuideEvent = emptyGuideEvent
                  , _events = M.empty
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

  -- Initialize database
  (initGuides, initGuardians, initEvents) <- initDb

  let app = App {view = view', update = update', inputs = [], initialState = initState { _guides = initGuides, _guardians = initGuardians, _events = initEvents } }

  -- Start main loop
  void . async $ do
    void $ runLoop app
    Gtk.mainQuit
  Gtk.main

  -- Save data?
  putStrLn "Saving data here?  Probably should do it in real time..."
  