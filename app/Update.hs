module Update where

import           GI.Gtk.Declarative.App.Simple
import           Lens.Micro

import           Model                        

update' :: Model -> Event -> Transition Model Event
update' m IncrAll             = Transition m 
                                $ return Nothing
update' m (GotoForm l f)      = Transition (set l f m) 
                                $ return Nothing
update' m NewGuide            = Transition m 
                                $ do
                                  putStrLn "Hello?"
                                  return $ Just $ GotoForm form FormGuide
update' m (ChangeText l n)    = Transition (set l n m)
                                $ return Nothing
update' _ Closed              = Exit
