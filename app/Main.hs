{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad                 (void)

import           GI.Gtk.Declarative.App.Simple

import           Model                         (Model(..), Form(..))
import           View                          (view')
import           Update                        (update')

initState :: Model
initState = Model { _form = FormMain 
                  , _guideName = "Zoe"
                  }

main :: IO ()
main = void $ run App
  { view         = view'
  , update       = update'
  , inputs       = []
  , initialState = initState
  }
