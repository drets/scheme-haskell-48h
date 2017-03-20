import           Control.Monad
import           Data.Either
import qualified Data.Vector as V
import           Test.HUnit
import           Text.Parsec

import           Scheme.Parser
import           Scheme.Eval
import           Scheme.Types


goodTestParse :: Test
goodTestParse = TestCase $ do
  let testCases =
        [ ( "1"
          , RealNumber (LispInteger 1)
          )
        , ( "10"
          , RealNumber (LispInteger 10)
          )
        , ( "-10.1"
          , RealNumber (LispDouble (-10.1))
          )
        , ( "1/2"
          , RealNumber (LispRational 1 2)
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
          , ComplexNumber (LispComplex (LispDouble 100.0) Positive (LispDouble 100.0))
          )
        , ( "100+100i"
          , ComplexNumber (LispComplex (LispInteger 100) Positive (LispInteger 100))
          )
        , ( "100.0-100i"
          , ComplexNumber (LispComplex (LispDouble 100.0) Negative (LispInteger 100))
          )
        , ( "(a test)"
          , List [Atom "a", Atom "test"]
          )
        , ( "(a (nested) test)"
          , List [Atom "a", List [Atom "nested"], Atom "test"]
          )
        , ( "(a (dotted . list) test)"
          , List [Atom "a", DottedList [Atom "dotted"] (Atom "list"), Atom "test"]
          )
        , ( "(a '(quoted (dotted . list)) test)"
          , List [Atom "a", List [Atom "quote", List [Atom "quoted", DottedList [Atom "dotted"] (Atom "list")]], Atom "test"]
          )
        , ( "`(a test)"
          , List [Atom "quasiquote", List [Atom "a", Atom "test"]]
          )
        , ( "\"\\r\""
          , String "\r"
          )
        , ( "'#(1 (1 2 3) 3)"
          , Vector (V.fromList [RealNumber (LispInteger 1),List [RealNumber (LispInteger 1),RealNumber (LispInteger 2),RealNumber (LispInteger 3)],RealNumber (LispInteger 3)])
          )
        ]

  forM_ testCases $ do
    \(input, expected) -> assertEqual "" (Right expected) (runParse input)

badTestParse :: Test
badTestParse = TestCase $ do
  let testCases =
        [ "(a '(imbalanced parens)"
        ]

  forM_ testCases $ do
    \input -> assert (isLeft (runParse input))

goodTestEval :: Test
goodTestEval = TestCase $ do
  let testCases =
        [
          ( "(+ 2 2)"
          , RealNumber (LispInteger 4)
          ),
          ( "(+ 2 (- 4 1))"
          , RealNumber (LispInteger 5)
          ),
          ( "(- (+ 4 6 3) 3 5 2)"
          , RealNumber (LispInteger 3)
          ),
          ( "(boolean? #f)"
          , Bool True
          ),
          ( "(boolean? 0)"
          , Bool False
          ),
          ("(boolean? '())"
          , Bool False
          ),
          ( "(string? \"Hello world\")"
          , Bool True
          ),
          ( "(string? #f)"
          , Bool False
          ),
          ( "(number? #f)"
          , Bool False
          ),
          ( "(number? 100)"
          , Bool True
          ),
          ( "(number? 100/10)"
          , Bool True
          ),
          ( "(number? 100+10i)"
          , Bool True
          ),
          ( "(number? 100-10i)"
          , Bool True
          ),
          ( "(symbol? 'foo)"
          , Bool True
          ),
          ( "(symbol? (car '(a b)))"
          , Bool True
          ),
          ( "(symbol? 'nil)"
          , Bool True
          ),
          ( "(symbol? \"bar\")"
          , Bool False
          ),
          ( "(symbol? '()))"
          , Bool False
          ),
          ( "(symbol? #f)"
          , Bool False
          ),
          ( "(symbol->string 'flying-fish)"
          , String "flying-fish"
          ),
          ( "(symbol->string 'Martin)"
          , String "martin"
          ),
          ( "(symbol->string (string->symbol \"Malvina\"))"
          , String "Malvina"
          )
        ]

  forM_ testCases $ do
    \(input, expected) -> assertEqual "" expected (runEval input)

runParse :: String -> Either ParseError LispValue
runParse = parse parseExpr "scheme"

runEval :: String -> LispValue
runEval input = case readExpr input of
  Right val -> case eval val of
    Right v  -> v
    Left err -> error $ show err
  Left err  -> error $ show err


main :: IO Counts
main = runTestTT $ TestList [goodTestParse, badTestParse, goodTestEval]
