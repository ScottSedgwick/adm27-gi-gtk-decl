module Update where

import           GI.Gtk.Declarative.App.Simple
import           Lens.Micro

import           Model                        

update' :: Model -> Event -> Transition Model Event
update' m IncrAll             = Transition m 
                                $ return Nothing
update' m (GotoForm f)        = Transition (set form f m) 
                                $ return Nothing
update' m NewGuide            = Transition m 
                                $ do
                                  putStrLn "Hello?"
                                  return $ Just $ GotoForm FormGuide

update' m NewGuardian         = Transition m $ return (Just $ GotoForm FormGuardian)
update' m NewGuideEvent       = Transition m $ return (Just $ GotoForm FormGuideEvent)
update' m CommitGuide         = Transition (commitCurrentGuide m)
                              $ return $ Just $ GotoForm FormMain
update' m (ChangeText l n)    = Transition (set l n m)
                                $ return Nothing
update' m (ChangeDate l n)    = Transition (set l n m)
                                $ return Nothing
update' m (ChangeBool l n)    = Transition (set l n m)
                                $ return Nothing
update' _ Closed              = Exit

commitCurrentGuide :: Model -> Model
commitCurrentGuide = undefined

-- revertCurrentGuide :: Model -> Model



-- Options: 
--  1) Create new guide, commit.  Add _currentGuide to _guides, set _currentGuide to emptyGuide.
--  2) Create new guide, revert.  Leave _guides as is, set _currentGuide to emptyGuide.
--  3) Edit guide, commit. Replace record in _guides with _currentGuide, set _currentGuide to emptyGuide.
--  4) Edit guide, revert. Leave _guides as is, set _currentGuide to emptyGuide.
-- Solution: I need an indexed data store for Guides (and everything else).
-- Option: IntMap, and an ID field?