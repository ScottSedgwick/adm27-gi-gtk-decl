{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE Rank2Types        #-}
module View where

import           GI.Gtk                        hiding (Bin, (:=), main, on)
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

import           GHC.Word                      (Word32)

import           Data.Text                     (Text)
import           Data.Time.Calendar            (Day(..), toGregorian, fromGregorian)
import           Lens.Micro

import           Model              
import           Adm27.Model           

-- An edit row and button row are each 39 high

mkEditRow :: FromWidget (Bin ListBoxRow) target => Model -> Text -> (Lens' Model Text -> Text -> Event) -> Lens' Model Text -> target Event
mkEditRow m label evt l = bin ListBoxRow [] 
                        $ container Box []
                        [ widget Label [ #label := label,   #widthRequest := 180 ]
                        , widget Entry [ #text := (m ^. l), #widthRequest := 300, onM #changed (fmap (evt l) . entryGetText) ]
                        ]

mkRow :: FromWidget (Bin ListBoxRow) target => Text -> Event -> target Event
mkRow label evt = bin ListBoxRow [] 
                $ container Box []
                [ widget Button [ #label := label, on #clicked evt, classes ["big-button"] ]
                ]

encodeDay :: (Word32, Word32, Word32) -> Day
encodeDay (y,m,d) = fromGregorian (fromIntegral y) (fromIntegral (m + 1)) (fromIntegral d)

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,y,_) = y

thd3 :: (a,b,c) -> c
thd3 (_,_,z) = z

-- This is less, not sure how much exactly
mkBoolRow :: FromWidget (Bin ListBoxRow) target => Model -> Text -> (Lens' Model Bool -> Bool -> Event) -> Lens' Model Bool -> target Event 
mkBoolRow m label evt l = bin ListBoxRow [] 
                        $ container Box []
                        [ widget Label [ #label := label, #widthRequest := 180 ]
                        , widget CheckButton [ #widthRequest   := 300
                                             , onM #clicked (fmap (evt l) . toggleButtonGetActive)
                                             , #active := (m ^. l)
                                             ]
                        ]

-- This is much more, not sure exactly how much
mkDateRow :: FromWidget (Bin ListBoxRow) target => Model -> Text -> (Lens' Model Day -> Day -> Event) -> Lens' Model Day -> target Event 
mkDateRow m label evt l = bin ListBoxRow [] 
                        $ container Box []
                        [ widget Label [ #label := label, #widthRequest := 180 ]
                        , widget Calendar [ #widthRequest   := 300
                                          , #day   := fromIntegral (thd3 (toGregorian (m ^. l)))
                                          , #month := fromIntegral (snd3 (toGregorian (m ^. l)) - 1)
                                          , #year  := fromIntegral (fst3 (toGregorian (m ^. l)))
                                          , onM #daySelected  (fmap (evt l . encodeDay) . calendarGetDate)
                                          , onM #monthChanged (fmap (evt l . encodeDay) . calendarGetDate) 
                                          ]
                        ]

view' :: Model -> AppView Window Event
view' st = bin Window [ #title := "ADM.27 Generator"
                      , on #deleteEvent (const (True, Closed))
                      , #widthRequest := 500
                      , #heightRequest := 858
                      ]
  $ case _form st of
      FormMain       -> mainForm st
      FormGuide      -> guideForm st
      FormGuardian   -> guardianForm st
      FormGuideEvent -> guideEventForm st
      
mainForm :: FromWidget (Bin ScrolledWindow) target => Model -> target Event
mainForm _ = bin ScrolledWindow [] $ container ListBox []
  [ mkRow "New Guide" NewGuide
  , mkRow "New Guardian" NewGuardian
  , mkRow "New Guide Event" NewGuideEvent 
  ]
      
guideForm :: FromWidget (Bin ScrolledWindow) target => Model -> target Event
guideForm m = bin ScrolledWindow [] $ container ListBox []
  [ mkEditRow m "GuideName" ChangeText (currentGuide . guide_name)
  , mkEditRow m "Guide Unit" ChangeText (currentGuide . guide_unit)
  , mkDateRow m "Date of Birth" ChangeDate (currentGuide . dob)
  , mkEditRow m "Medicare Number" ChangeText (currentGuide . medicareNumber)
  , mkEditRow m "Medicare Reference" ChangeText (currentGuide . medicareReference)
  , mkEditRow m "Medicare Address" ChangeText (currentGuide . medicareAddress)
  , mkBoolRow m "Private Health Cover" ChangeBool (currentGuide . privateHealthCover)
  , mkEditRow m "Private Health Fund" ChangeText (currentGuide . privateHealthFund)
  , mkEditRow m "Private Fund Number" ChangeText (currentGuide . privateHealthNumber)
  , mkBoolRow m "Ambulance Cover" ChangeBool (currentGuide . ambulanceCover)
  , mkEditRow m "Medical Emergency Contact" ChangeText (currentGuide . medicalEmergencyContact)
  , mkEditRow m "Medical Emergency Phone" ChangeText (currentGuide . medicalEmergencyPhone)
  , mkEditRow m "Medical Emergency Mobile" ChangeText (currentGuide . medicalEmergencyMobile)
  , mkEditRow m "Guide Membership Number" ChangeText (currentGuide . guideMembershipNumber)
  , mkDateRow m "Guide Membership Expiry" ChangeDate (currentGuide . guideMembershipExpiry)
  , mkBoolRow m "Taking Medication" ChangeBool (currentGuide . takingMedication)
  , mkEditRow m "First Aider Info" ChangeText (currentGuide . firstAiderInfo)
  , mkBoolRow m "Contact Lenses" ChangeBool (currentGuide . contactLenses)
  , mkDateRow m "Last Tetanus Shot" ChangeDate (currentGuide . lastTetanusShot)
  , mkEditRow m "Allergy Details" ChangeText (currentGuide . allergyDetails)
  , mkBoolRow m "Chronic Illness" ChangeBool (currentGuide . chronicIllness)
  , mkEditRow m "Chronic Illness Details" ChangeText (currentGuide . chronicIllnessDetails)
  , mkBoolRow m "Knows About Menstruation" ChangeBool (currentGuide . knowsAboutMenstruation)
  , mkEditRow m "Special Food Requirements" ChangeText (currentGuide . specialFoodRequirements)
  , mkBoolRow m "Can Take Paracetamol" ChangeBool (currentGuide . canTakeParacetamol)
  , mkEditRow m "Swimming Distance" ChangeText (currentGuide . swimmingDistance)
  , mkBoolRow m "Suffers Asthma" ChangeBool (currentGuide . sufferAsthma)
  , mkBoolRow m "Suffers Bed Wetting" ChangeBool (currentGuide . sufferBedWetting)
  , mkBoolRow m "Suffers Diabetes" ChangeBool (currentGuide . sufferDiabetes)
  , mkBoolRow m "Suffers Epilepsy" ChangeBool (currentGuide . sufferEpilepsy)
  , mkBoolRow m "Suffers Sleepwalking" ChangeBool (currentGuide . sufferSleepwalking)
  , mkBoolRow m "Suffers Fainting" ChangeBool (currentGuide . sufferFainting)
  , mkBoolRow m "Suffers Hat Fever" ChangeBool (currentGuide . sufferHayFever)
  , mkBoolRow m "Suffers Nosebleeds" ChangeBool (currentGuide . sufferNosebleeds)
  , mkBoolRow m "Suffers Sever Allergies" ChangeBool (currentGuide . sufferSevereAllergies)
  , mkEditRow m "Activity Exceptions" ChangeText (currentGuide . activityExceptions)
  , mkRow "Commit" CommitGuide
  , mkRow "Cancel" (GotoForm FormMain) 
  ]
    
guardianForm :: FromWidget (Bin ScrolledWindow) target => Model -> target Event
guardianForm m = bin ScrolledWindow [] $ container ListBox []
  [ mkEditRow m "Guardian Name" ChangeText (currentGuardian . guardian_name) 
  , mkEditRow m "Address" ChangeText (currentGuardian . address) 
  , mkEditRow m "State" ChangeText (currentGuardian . state) 
  , mkEditRow m "Postcode" ChangeText (currentGuardian . postcode) 
  , mkEditRow m "Business Phone" ChangeText (currentGuardian . businessPhone) 
  , mkEditRow m "Home Phone" ChangeText (currentGuardian . homePhone) 
  , mkEditRow m "Mobile Phone" ChangeText (currentGuardian . mobilePhone) 
  , mkRow "Cancel" (GotoForm FormMain) 
  ]

guideEventForm :: FromWidget (Bin ScrolledWindow) target => Model -> target Event
guideEventForm m = bin ScrolledWindow [] $ container ListBox []
  [ mkEditRow m "Activity Name" ChangeText (currentGuideEvent . activity_name)
  , mkDateRow m "Start Date" ChangeDate (currentGuideEvent . dateStart)
  , mkDateRow m "End Date" ChangeDate (currentGuideEvent . dateEnd)
  , mkEditRow m "Fee Enclosed" ChangeText (currentGuideEvent . feeEnclosed)
  , mkEditRow m "Location" ChangeText (currentGuideEvent . location)
  , mkEditRow m "Leader" ChangeText (currentGuideEvent . leader)
  , mkEditRow m "Emergency Contact" ChangeText (currentGuideEvent . emergencyContact)
  , mkEditRow m "Emergency Phone" ChangeText (currentGuideEvent . emergencyPhone)
  , mkEditRow m "Total Cost" ChangeText (currentGuideEvent . totalCost)
  , mkEditRow m "Deposit" ChangeText (currentGuideEvent . deposit)
  , mkDateRow m "Deposit Due" ChangeDate (currentGuideEvent . depositDue)
  , mkEditRow m "Balance" ChangeText (currentGuideEvent . balance)
  , mkDateRow m "Balance Due" ChangeDate (currentGuideEvent . balanceDue)
  , mkEditRow m "Activities" ChangeText (currentGuideEvent . activities)
  , mkEditRow m "Travel Arrangements" ChangeText (currentGuideEvent . travelArrangements)
  , mkRow "Cancel" (GotoForm FormMain) 
  ]