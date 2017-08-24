{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lib where

import Data.String.Conversions(cs)
import Data.Text (Text, concat)
import Prelude hiding (concat, try)
import Text.Megaparsec
       ((<|>), anyChar, char, choice, count, many, manyTill, noneOf, sepBy, some,
        string, try)
import Text.Megaparsec.Text (Parser,)
--import Text.Mega

newtype Message = Message
    { msgSegments :: [Segment]
    } deriving (Show)

newtype Segment = Segment
    { segFields :: [Field]
    } deriving (Show)


data MSH = MSH
    { mshEncodingChars :: EncodingCharacters
    , hl7MessageType :: MessageType
    , mshFields :: [Field]
    } deriving (Eq, Show)

data EncodingCharacters = EncodingCharacters
    { hl7FieldSeperator :: Char
    , hl7ComponentSeperator :: Char
    , hl7SubcomponentSeperator :: Char
    , hl7RepetitionSeperator :: Char
    , hl7EscapeCharacter :: Char
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
    deriving (Eq, Read, Show)

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

newtype Field = Field {
    components :: [Component]
} deriving (Eq, Show)

data Component = Component {
      subcomponents :: [Subcomponent]          
    }    deriving (Eq, Show)

type Subcomponent = Text

parseMSH :: Parser MSH
parseMSH = do
    string "MSH"
    hl7FieldSeperator <- anyChar
    msh2 <- take 4 <$> count 5 anyChar
    -- This should be safe since count n will always return n elements or fail
    let [hl7ComponentSeperator, hl7RepetitionSeperator, hl7EscapeCharacter, hl7SubcomponentSeperator] =
            msh2
        mshEncodingChars = EncodingCharacters {..}
    fields <- parseFields mshEncodingChars
    let mshFields =
            (map
                 (Field . return . Component . return . cs)
                 ([hl7FieldSeperator] : [msh2])) ++
            fields
        hl7MessageType = read $ cs $ (subcomponents $ (components $ mshFields !! 8) !! 0) !! 0
    return MSH {..}

parseFields :: EncodingCharacters -> Parser [Field]
parseFields enc@EncodingCharacters {..} = do
    let fieldParser :: Parser Field
        fieldParser = (Field <$> parseComponents enc)
    sepBy fieldParser (char hl7FieldSeperator)

validChar :: EncodingCharacters -> Parser Char
validChar EncodingCharacters {..} =
    noneOf
        [ hl7FieldSeperator
        , hl7ComponentSeperator
        , hl7SubcomponentSeperator
        , hl7EscapeCharacter
        , hl7RepetitionSeperator
        ]

 -- TODO: Handle remaining escape sequences
unescape :: EncodingCharacters -> Parser Text
unescape enc@EncodingCharacters {..} =
    concat <$> (some $ choice $ zipWith replaceEscSequence escapeSequences encodingChars ++ [(cs <$> (some $ validChar enc))])
  where
    replaceEscSequence :: String -> String -> Parser Text
    replaceEscSequence sequence char = string sequence >> (return $ cs $ char)
    encodingChars =
        [ [hl7FieldSeperator]
        , [hl7ComponentSeperator]
        , [hl7SubcomponentSeperator]
        , [hl7EscapeCharacter]
        , [hl7RepetitionSeperator]
        ]
    escapeSequences = ["\\F\\", "\\S\\", "\\T\\", "\\E\\", "\\R\\"]

parseComponents :: EncodingCharacters -> Parser [Component]
parseComponents enc@EncodingCharacters {..} = do
    let componentParser :: Parser Component
        componentParser = (Component <$> parseSubcomponents enc)
    sepBy componentParser (char hl7ComponentSeperator)
    
parseSubcomponents :: EncodingCharacters -> Parser [Subcomponent]
parseSubcomponents enc@EncodingCharacters{..} =
    sepBy (unescape enc) (char hl7SubcomponentSeperator)
