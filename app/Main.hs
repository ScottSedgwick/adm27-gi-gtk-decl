{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent.Async      (async)
import           Control.Monad                 (void)
import           Data.ByteString               (ByteString)

import qualified GI.Gdk                        as Gdk
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative.App.Simple

import           Model                         (Model(..), Form(..))
import           View                          (view')
import           Update                        (update')

initState :: Model
initState = Model { _form = FormMain 
                  , _guideName = "Zoe"
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

  -- Start main loop
  void . async $ do
    void $ runLoop app
    Gtk.mainQuit
  Gtk.main
 where
  app = App {view = view', update = update', inputs = [], initialState = initState}