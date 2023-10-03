{-# LANGUAGE GADTs #-}
module TypeCheckerSpec where

import Data.Function ((&))
import Data.String.Interpolate
import Data.Text.Lazy qualified as TL

import LuQL.Compiler
import LuQL.Parser

import Test.Syd

import Tests.Utils (models)

import Text.Pretty.Simple

spec :: Spec
spec =  do
  describe "group by" $ do
    it "fails to compile if an field not kept by a group by is used" $ do
      (parseQuery [__i|
        from Languages
        group by language.language_id {}
        where language.name == "spanish"
      |] & either (error . show) id
         & compileProgram models
         & either id (error . TL.unpack . mappend "Successful compilation: " . pShow)
         )
         `shouldBe`
            [ CompilerError "Ref (54,62) \"language\" no tiene una propiedad: name" ( 62 , 68 )
            ]

    it "fails to compile if the group by doesn't keep the model field to join by" $ do
      -- TODO I'd like a better error for this, like "the city model you have
      -- is missing the city_id field which is necessary for the join with
      -- Countries"
      (parseQuery [__i|
        from Cities
        group by city.city {}
        join Countries
      |] & either (error . show) id
         & compileProgram models
         & either id (error . TL.unpack . mappend "Successful compilation: " . pShow)
         )
         `shouldBe`
            [ CompilerError "no model to join available" ( 34 , 48 )
            ]
