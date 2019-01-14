{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Model where

import           Data.Text                     (Text)
import           Lens.Micro.TH                 (makeLenses)
import           Lens.Micro
import           Database.Selda

data Form = FormMain
          | FormGuide
          | FormGuardian
          | FormActivity
          deriving (Show, Eq)

data Guide = Guide
  { _guide_name :: Text
  , _guide_unit :: Text
  , _dob :: Text
  , _medicareNumber :: Text
  , _medicareReference :: Text
  , _medicareAddress :: Text
  , _privateHealthCover :: Bool
  , _privateHealthFund :: Text
  , _privateHealthNumber :: Text
  , _ambulanceCover :: Bool
  , _medicalEmergencyContact :: Text
  , _medicalEmergencyPhone :: Text
  , _medicalEmergencyMobile :: Text
  , _guideMembershipNumber :: Text
  , _guideMembershipExpiry :: Text
  , _takingMedication :: Bool
  , _firstAiderInfo :: Text
  , _contactLenses :: Bool
  , _lastTetanusShot :: Text
  , _allergyDetails :: Text
  , _chronicIllness :: Bool
  , _chronicIllnessDetails :: Text
  , _knowsAboutMenstruation :: Bool
  , _specialFoodRequirements :: Text
  , _canTakeParacetamol :: Bool
  , _swimmingDistance :: Text
  , _sufferAsthma :: Bool
  , _sufferBedWetting :: Bool
  , _sufferDiabetes :: Bool
  , _sufferEpilepsy :: Bool
  , _sufferSleepwalking :: Bool
  , _sufferFainting :: Bool
  , _sufferHayFever :: Bool
  , _sufferNosebleeds :: Bool
  , _sufferSevereAllergies :: Bool
  , _activityExceptions :: Text
  } deriving (Show, Eq, Generic)
instance SqlRow Guide
makeLenses ''Guide

emptyGuide :: Guide
emptyGuide = Guide
  { _guide_name = ""
  , _guide_unit = ""
  , _dob = ""
  , _medicareNumber = ""
  , _medicareReference = ""
  , _medicareAddress = ""
  , _privateHealthCover = False
  , _privateHealthFund = ""
  , _privateHealthNumber = ""
  , _ambulanceCover = False
  , _medicalEmergencyContact = ""
  , _medicalEmergencyPhone = ""
  , _medicalEmergencyMobile = ""
  , _guideMembershipNumber = ""
  , _guideMembershipExpiry = ""
  , _takingMedication = False
  , _firstAiderInfo = ""
  , _contactLenses = False
  , _lastTetanusShot = ""
  , _allergyDetails = ""
  , _chronicIllness = False
  , _chronicIllnessDetails = ""
  , _knowsAboutMenstruation = False
  , _specialFoodRequirements = ""
  , _canTakeParacetamol = False
  , _swimmingDistance = ""
  , _sufferAsthma = False
  , _sufferBedWetting = False
  , _sufferDiabetes = False
  , _sufferEpilepsy = False
  , _sufferSleepwalking = False
  , _sufferFainting = False
  , _sufferHayFever = False
  , _sufferNosebleeds = False
  , _sufferSevereAllergies = False
  , _activityExceptions = ""
  }

instance Ord Guide where
  compare x y = compare (_guide_name x) (_guide_name y)
  (<=) x y = (_guide_name x) <= (_guide_name y)

data Guardian = Guardian
  { _guardian_name :: Text
  , _address :: Text
  , _state :: Text
  , _postcode :: Text
  , _businessPhone :: Text
  , _homePhone :: Text
  , _mobilePhone :: Text
  } deriving (Show, Eq, Generic)
instance SqlRow Guardian
makeLenses ''Guardian

instance Ord Guardian where
  compare x y = compare (_guardian_name x) (_guardian_name y)
  (<=) x y = (_guardian_name x) <= (_guardian_name y)

data GuideEvent = GuideEvent
  { _activity_name :: Text
  , _dateStart :: Text
  , _dateEnd :: Text
  , _feeEnclosed :: Text
  , _location :: Text
  , _leader :: Text
  , _emergencyContact :: Text
  , _emergencyPhone :: Text
  , _totalCost :: Text
  , _deposit :: Text
  , _depositDue :: Text
  , _balance :: Text
  , _balanceDue :: Text
  , _activities :: Text
  , _travelArrangements :: Text
  } deriving (Show, Eq, Generic)
instance SqlRow GuideEvent
makeLenses ''GuideEvent

instance Ord GuideEvent where
  compare x y = compare (_activity_name x) (_activity_name y)
  (<=) x y = (_activity_name x) <= (_activity_name y)

data Model = Model
  { _form :: Form
  , _currentGuide :: Guide
  , _guides :: [Guide]
  } deriving (Show, Eq)
makeLenses ''Model

data Event = IncrAll
            | GotoForm   (Lens' Model Form) Form
            | ChangeText (Lens' Model Text) Text
            | NewGuide
            | Closed
