{-# LANGUAGE OverloadedStrings #-}
module ParserSpec where

import Test.Hspec
import Text.Megaparsec
import Text.Megaparsec.Text

import Lib

spec :: Spec
spec = do
    describe "The Parser" $ do
        it "can parse a message" $ do
            let val =
                    runParser
                        parseMSH
                        ""
                        "MSH|^~\&|MegaReg|XYZHospC|SuperOE|XYZImgCtr|20060529090131-0500||ADT^A01^ADT_A01|01052901|P|2.5"
            val `shouldBe`
                (Right $
                 MSH
                 { hl7FieldSeperator = '|'
                 , hl7ComponentSeperator = '^'
                 , hl7SubcomponentSeperator = '\\'
                 , hl7RepetitionSeperator = '&'
                 , hl7MessageType = ADT
                 , mshFields =
                       [ FieldValue "|"
                       , FieldValue "^\\&"
                       , FieldValue "MegaReg"
                       , FieldValue "XYZHospC"
                       , FieldValue "SuperOE"
                       , FieldValue "XYZImgCtr"
                       , FieldValue "20060529090131-0500|"
                       , Components
                             [ ComponentValue "ADT"
                             , ComponentValue "A01"
                             , ComponentValue "ADT_A01"
                             , ComponentValue "01052901"
                             , ComponentValue "P"
                             , ComponentValue "2.5"
                             ]
                       ]
                 })
