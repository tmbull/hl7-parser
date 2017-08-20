{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Data.Text (Text)
import Text.Megaparsec.Text (Parser)
--import Text.Mega

newtype Message = Message
    { msgSegments :: [Segment]
    } deriving (Show)

newtype Segment = Segment
    { segFields :: [Field]
    } deriving (Show)

parseMSH :: Parser MSH
parseMSH = undefined

data MSH = MSH
    { hl7FieldSeperator :: Char
    , hl7ComponentSeperator :: Char
    , hl7SubcomponentSeperator :: Char
    , hl7RepetitionSeperator :: Char
    , hl7MessageType :: MessageType
    , mshFields :: [Field]
    } deriving (Eq, Show)

data MessageType
    = ACK
    | ADT
    | BAR
    | DFT
    | MDM
    | MFN
    | ORM
    | ORU
    | QRY
    | RAS
    | RDE
    | RGV
    | SIU
    deriving (Eq, Show)

getMessageTypeDescription :: MessageType -> Text
getMessageTypeDescription ACK = "General acknowledgement"
getMessageTypeDescription ADT = "Admitdischarge transer"
getMessageTypeDescription BAR = "Add/change billing account"
getMessageTypeDescription DFT = "Detailed financial transaction"
getMessageTypeDescription MDM = "Medical document management"
getMessageTypeDescription MFN = "Master files notification"
getMessageTypeDescription ORM = "Order (Pharmacy/treatment)"
getMessageTypeDescription ORU = "Observation result (unsolicited)"
getMessageTypeDescription QRY = "Query, original mode"
getMessageTypeDescription RAS = "Pharmacy/treatment administration"
getMessageTypeDescription RDE = "Pharmacy/treatment encoded order"
getMessageTypeDescription RGV = "Pharmacy/treatment give"
getMessageTypeDescription SIU = "Scheduling information unsolicited"

data Field
    = FieldValue Value
    | Components [Component]
    deriving (Eq, Show)

data Component
    = ComponentValue Value
    | Subcomponents [Value]
    deriving (Eq, Show)

type Value = Text

parse = "foo"
