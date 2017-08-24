{-# LANGUAGE OverloadedStrings #-}
module ParserSpec where

import Test.Hspec
import Text.Megaparsec
import Text.Megaparsec.Text

import Lib

spec :: Spec
spec = do
    describe "unescape logic" $ do
        it "Should work" $ do
            let val =
                    runParser
                        (unescape
                        EncodingCharacters
                        { hl7FieldSeperator = '|'
                        , hl7ComponentSeperator = '^'
                        , hl7SubcomponentSeperator = '&'
                        , hl7RepetitionSeperator = '~'
                        , hl7EscapeCharacter = '\\'
                        })
                        ""
                        "\\R\\"
            val `shouldBe` (Right "~")
    describe "MSH Parser" $ do
        it "should work on a longer string" $ do
            let val =
                    runParser
                        (unescape
                        EncodingCharacters
                        { hl7FieldSeperator = '|'
                        , hl7ComponentSeperator = '^'
                        , hl7SubcomponentSeperator = '&'
                        , hl7RepetitionSeperator = '~'
                        , hl7EscapeCharacter = '\\'
                        })
                        ""
                        "abc\\R\\123"
            val `shouldBe` Right "abc~123"
        it "can parse an MSH segment" $ do
            let val =
                    runParser
                        parseMSH
                        ""
                        "MSH|^~\\&|MegaReg|XYZHospC|SuperOE|XYZImgCtr|20060529090131-0500||ADT^A01^ADT_A01|01052901|P|2.5"
            val `shouldBe`
                (Right $
                 MSH
                 { mshEncodingChars =
                       EncodingCharacters
                       { hl7FieldSeperator = '|'
                       , hl7ComponentSeperator = '^'
                       , hl7SubcomponentSeperator = '&'
                       , hl7RepetitionSeperator = '~'
                       , hl7EscapeCharacter = '\\'
                       }
                 , hl7MessageType = ADT
                 , mshFields =
                       [ Field
                         {components = [Component {subcomponents = ["|"]}]}
                       , Field
                         {components = [Component {subcomponents = ["^~\\&"]}]}
                       , Field
                         { components =
                               [Component {subcomponents = ["MegaReg"]}]
                         }
                       , Field
                         { components =
                               [Component {subcomponents = ["XYZHospC"]}]
                         }
                       , Field
                         { components =
                               [Component {subcomponents = ["SuperOE"]}]
                         }
                       , Field
                         { components =
                               [Component {subcomponents = ["XYZImgCtr"]}]
                         }
                       , Field
                         { components =
                               [ Component
                                 {subcomponents = ["20060529090131-0500"]}
                               ]
                         }
                       , Field {components = [Component {subcomponents = []}]}
                       , Field
                         { components =
                               [ Component {subcomponents = ["ADT"]}
                               , Component {subcomponents = ["A01"]}
                               , Component {subcomponents = ["ADT_A01"]}
                               ]
                         }
                       , Field
                         { components =
                               [Component {subcomponents = ["01052901"]}]
                         }
                       , Field
                         {components = [Component {subcomponents = ["P"]}]}
                       , Field
                         {components = [Component {subcomponents = ["2.5"]}]}
                       ]
                 })
        it "can parse a segment containing an escape character" $ do
            let val =
                    runParser
                        parseMSH
                        ""
                        "MSH|^~\\&|MegaReg|XYZHospC|SuperOE|XYZImgCtr|20060529090131\\R\\0500||ADT^A01^ADT_A01|01052901|P|2.5"
            val `shouldBe`
                (Right $
                 MSH
                 { mshEncodingChars =
                       EncodingCharacters
                       { hl7FieldSeperator = '|'
                       , hl7ComponentSeperator = '^'
                       , hl7SubcomponentSeperator = '&'
                       , hl7RepetitionSeperator = '~'
                       , hl7EscapeCharacter = '\\'
                       }
                 , hl7MessageType = ADT
                 , mshFields =
                       [ Field
                         {components = [Component {subcomponents = ["|"]}]}
                       , Field
                         {components = [Component {subcomponents = ["^~\\&"]}]}
                       , Field
                         { components =
                               [Component {subcomponents = ["MegaReg"]}]
                         }
                       , Field
                         { components =
                               [Component {subcomponents = ["XYZHospC"]}]
                         }
                       , Field
                         { components =
                               [Component {subcomponents = ["SuperOE"]}]
                         }
                       , Field
                         { components =
                               [Component {subcomponents = ["XYZImgCtr"]}]
                         }
                       , Field
                         { components =
                               [ Component
                                 {subcomponents = ["20060529090131~0500"]}
                               ]
                         }
                       , Field {components = [Component {subcomponents = []}]}
                       , Field
                         { components =
                               [ Component {subcomponents = ["ADT"]}
                               , Component {subcomponents = ["A01"]}
                               , Component {subcomponents = ["ADT_A01"]}
                               ]
                         }
                       , Field
                         { components =
                               [Component {subcomponents = ["01052901"]}]
                         }
                       , Field
                         {components = [Component {subcomponents = ["P"]}]}
                       , Field
                         {components = [Component {subcomponents = ["2.5"]}]}
                       ]
                 })
