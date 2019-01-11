module Update where

import           GI.Gtk.Declarative.App.Simple
import           Lens.Micro

import           Model                         (Model(..), Event(..), Form(..))

update' :: Model -> Event -> Transition Model Event
update' m IncrAll             = Transition m                              (return Nothing)
-- update' m GotoMain            = Transition (m { _form      = FormMain  }) (return Nothing)
update' m (GotoForm l f)      = Transition (set l f m)                    (return Nothing)
update' m NewGuide            = Transition (m { _form      = FormGuide }) (return Nothing)
update' m (ChangeText l n)    = Transition (set l n m)                    (return Nothing)
update' _ Closed              = Exit

-- (.~) :: ASetter s t a b -> b -> s -> t
-- set :: ASetter s t a b -> b -> s -> t