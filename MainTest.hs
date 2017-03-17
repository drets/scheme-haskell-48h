module TestMain where

import Test.HUnit
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces, try)

import Main hiding (main)


testScheme :: Test
testScheme = TestCase $ do
  let testCases =
        [ ( "1"
          , RealNumber (LispInteger 1)
          )
        , ( "10"
          , RealNumber (LispInteger 10)
          )
        , ( "10."
          , RealNumber (LispDouble 10.0)
          )
        , ( "1/2"
          , RationalNumber (LispRational 1 2)
          )
        , ( "var"
          , Atom "var"
          )
        , ( "\"Hello world\""
          , String "Hello world"
          )
        , ( "\"Hello \\\"Haskell\\\" world\""
          , String "Hello \"Haskell\" world"
          )
        , ( "#\\space"
          , Character "space"
          )
        , ( "#\\ "
          , Character " "
          )
        , ( "#d100.0"
          , RealNumber (LispDouble 100.0)
          )
        , ( "#b100"
          , RealNumber (LispInteger 4)
          )
        , ( "#o100"
          , RealNumber (LispInteger 64)
          )
        , ( "#x100"
          , RealNumber (LispInteger 256)
          )
        , ( "#t"
          , Bool True
          )
        , ( "#f"
          , Bool False
          )
        , ( "100.0+100.0i"
          , ComplexNumber (LispComplex (LispDouble 100.0) (LispDouble 100.0))
          )
        , ( "100+100i"
          , ComplexNumber (LispComplex (LispInteger 100) (LispInteger 100))
          )
        , ( "100.0+100i"
          , ComplexNumber (LispComplex (LispDouble 100.0) (LispInteger 100))
          )
        ]

  forM_ testCases $ do
    \(input, expected) -> assertEqual "" (Right expected) (run input)

run :: String -> Either ParseError LispValue
run = parse parseExpr "scheme"

main :: IO Counts
main = runTestTT $ TestList [testScheme]
