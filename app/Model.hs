{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Model where

import           Data.Time.Calendar            (Day(..), fromGregorian)
import           Data.Hashable                 (Hashable(..))
import qualified Data.IntMap                   as M
import           Data.Text                     (Text)
import           Lens.Micro.TH                 (makeLenses)
import           Lens.Micro
import           Database.Selda

data Form = FormMain
          | FormGuide
          | FormGuardian
          | FormGuideEvent
          deriving (Show, Eq)

data Guide = Guide
  { _guide_id :: Int
  , _guide_name :: Text
  , _guide_unit :: Text
  , _dob :: Day
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
  , _guideMembershipExpiry :: Day
  , _takingMedication :: Bool
  , _firstAiderInfo :: Text
  , _contactLenses :: Bool
  , _lastTetanusShot :: Day
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
makeLenses ''Guide

instance SqlRow Guide

instance Hashable Guide where
  hash = _guide_id
  hashWithSalt s g = s + hash g

emptyGuide :: Guide
emptyGuide = Guide
  { _guide_id = 0
  , _guide_name = ""
  , _guide_unit = ""
  , _dob = fromGregorian 2005 1 1
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
  , _guideMembershipExpiry = fromGregorian 2020 1 1
  , _takingMedication = False
  , _firstAiderInfo = ""
  , _contactLenses = False
  , _lastTetanusShot = fromGregorian 2016 1 1
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
  (<=) x y = _guide_name x <= _guide_name y

data Guardian = Guardian
  { _guardian_id :: Int
  , _guardian_name :: Text
  , _address :: Text
  , _state :: Text
  , _postcode :: Text
  , _businessPhone :: Text
  , _homePhone :: Text
  , _mobilePhone :: Text
  } deriving (Show, Eq, Generic)
makeLenses ''Guardian

instance SqlRow Guardian

instance Hashable Guardian where
  hash = _guardian_id
  hashWithSalt s g = s + hash g

instance Ord Guardian where
  compare x y = compare (_guardian_name x) (_guardian_name y)
  (<=) x y = _guardian_name x <= _guardian_name y

emptyGuardian :: Guardian
emptyGuardian = Guardian
  { _guardian_id = 0
  , _guardian_name = ""
  , _address = ""
  , _state = ""
  , _postcode = ""
  , _businessPhone = ""
  , _homePhone = ""
  , _mobilePhone = ""
  }

data GuideEvent = GuideEvent
  { _event_id :: Int
  , _activity_name :: Text
  , _dateStart :: Day
  , _dateEnd :: Day
  , _feeEnclosed :: Text
  , _location :: Text
  , _leader :: Text
  , _emergencyContact :: Text
  , _emergencyPhone :: Text
  , _totalCost :: Text
  , _deposit :: Text
  , _depositDue :: Day
  , _balance :: Text
  , _balanceDue :: Day
  , _activities :: Text
  , _travelArrangements :: Text
  } deriving (Show, Eq, Generic)
makeLenses ''GuideEvent

instance SqlRow GuideEvent

instance Hashable GuideEvent where
  hash = _event_id
  hashWithSalt s g = s + hash g

instance Ord GuideEvent where
  compare x y = compare (_activity_name x) (_activity_name y)
  (<=) x y = _activity_name x <= _activity_name y

emptyGuideEvent :: GuideEvent
emptyGuideEvent = GuideEvent
  { _event_id = 0
  , _activity_name = ""
  , _dateStart = fromGregorian 2019 1 1
  , _dateEnd = fromGregorian 2019 1 1
  , _feeEnclosed = ""
  , _location = ""
  , _leader = ""
  , _emergencyContact = ""
  , _emergencyPhone = ""
  , _totalCost = ""
  , _deposit = ""
  , _depositDue = fromGregorian 2019 1 1
  , _balance = ""
  , _balanceDue = fromGregorian 2019 1 1
  , _activities = ""
  , _travelArrangements = ""
  }

data Model = Model
  { _form :: Form
  , _currentGuide :: Guide
  , _guides    :: M.IntMap Guide
  , _currentGuardian :: Guardian
  , _guardians :: M.IntMap Guardian
  , _currentGuideEvent :: GuideEvent
  , _events    :: M.IntMap GuideEvent
  } deriving (Show, Eq)
makeLenses ''Model

data Event = IncrAll
            | GotoForm   Form
            | ChangeText (Lens' Model Text) Text
            | ChangeDate (Lens' Model Day)  Day
            | ChangeBool (Lens' Model Bool) Bool
            | NewGuide
            | NewGuardian
            | NewGuideEvent
            | CommitGuide
            | Closed
