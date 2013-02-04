{-# LANGUAGE DeriveDataTypeable,DeriveGeneric #-}
module Types where

import Control.Distributed.Process
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

type IMEI = Integer

{-|
   A MSISDN is limited to 15 or 16 digits, prefixes not included (e.g., 00 prefixes an international MSISDN when dialing from Sweden).
   In GSM and its variant DCS 1800, MSISDN is built up as
   MSISDN = CC + NDC + SN
   CC = Country Code
   NDC = National Destination Code, identifies one or part of a PLMN
   SN = Subscriber Number

   MSISDN: 380561234567
   CC	380	Ukraine
   NDC	56	Dnipropetrovsk
   SN	1234567	Subscriber's number
-}

type MSCAddr = ProcessId

type Cell = Int

data MSISDN =
  MSISDN { cc :: Int 
         , ndc :: Int
         , sn  :: Int
         } deriving (Show,Eq,Typeable,Generic)

data ICCID =
  ICCID { iin :: Int
        , iai :: Integer
        , chk :: Int
        } deriving (Show,Typeable,Generic)

data IMSI = 
  IMSI { mcc :: Int
       , mnc :: Int
       , msin :: Int
       } deriving (Eq,Show,Typeable,Generic)

data SIM = 
  SIM { iccid :: ICCID
      , s_imsi  :: IMSI
      , ki    :: Integer -- Authentication Key
      , lai   :: Integer -- Location Area Identity
      } deriving (Show,Typeable,Generic)

data MH = 
  MH { imei :: IMEI
     , mh_msisdn :: MSISDN
     , sim :: SIM
     } deriving (Show,Typeable,Generic)

instance Binary MH

data HLREntry = 
  HLREntry { h_imsi :: IMSI -- primary key
           , h_msisdn :: MSISDN -- primary key
           , vlrAddr :: Maybe MSCAddr
           , h_cellNumber :: Cell
           } deriving (Eq,Show,Typeable,Generic)

data VLREntry =
  VLREntry { v_imsi :: IMSI
           , v_msisdn :: MSISDN
           , hlrAddr :: MSCAddr
           , v_cellNumber :: Cell
           } deriving (Show,Typeable,Generic)


type HLR = [HLREntry]

type VLR = [VLREntry]

data MSC =
  MSC { addr :: MSCAddr
      , hlr :: HLR
      , vlr :: VLR
      } deriving (Show,Typeable,Generic)

instance Binary MSISDN
instance Binary HLREntry
instance Binary VLREntry
instance Binary MSC