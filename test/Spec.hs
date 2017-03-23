import           Control.Monad
import           Control.Monad.Except
import           Data.Either
import           Data.IORef
import qualified Data.Vector as V
import           System.IO.Unsafe
import           Test.HUnit
import           Text.Parsec

import           Scheme.Parser
import           Scheme.Eval
import           Scheme.Types


goodTestParse :: Env -> Test
goodTestParse env = TestCase $ do
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
          , Character ' '
          )
        , ( "#\\ "
          , Character ' '
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
        , ( "\"\""
          , String ""
          )
        , ( "'#(1 (1 2 3) 3)"
          , Vector (V.fromList [RealNumber (LispInteger 1),List [RealNumber (LispInteger 1),RealNumber (LispInteger 2),RealNumber (LispInteger 3)],RealNumber (LispInteger 3)])
          )
        ]

  forM_ testCases $ do
    \(input, expected) -> assertEqual "" (Right expected) (runParse input)

badTestParse :: Env -> Test
badTestParse env = TestCase $ do
  let testCases =
        [ "(a '(imbalanced parens)"
        ]

  forM_ testCases $ do
    \input -> assert (isLeft (runParse input))

goodTestEval :: Env -> Test
goodTestEval env = TestCase $ do
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
          ),
          ( "(if (> 2 3) \"no\" \"yes\")"
          , String "yes"
          ),
          ( "(if (= 3 3) (+ 2 3 (- 5 1)) \"unequal\")"
          , RealNumber (LispInteger 9)
          ),
          ( "(cdr '(a simple test))"
          , List [Atom "simple", Atom "test"]
          ),
          ( "(car (cdr '(a simple test)))"
          , Atom "simple"
          ),
          ( "(car '((this is) a test))"
          , List [Atom "this", Atom "is"]
          ),
          ( "(cons '(this is) 'test)"
          , DottedList [List [Atom "this",Atom "is"]] (Atom "test")
          ),
          ( "(cons '(this is) '())"
          , List [List [Atom "this",Atom "is"]]
          ),
          ( "(eqv? 1 3)"
          , Bool False
          ),
          ( "(eqv? 3 3)"
          , Bool True
          ),
          ( "(eqv? 'atom 'atom)"
          , Bool True
          ),
          ( "(cond ((> 3 2) 'greater) (< 3 2) 'less)"
          , Atom "greater"
          ),
          ( "(cond ((> 3 2) 'g 'great 'greater) (< 3 2) 'less)"
          , Atom "greater"
          ),
          ( "(cond ((< 3 2) 'less) ((= 3 3) 'equal))"
          , Atom "equal"
          ),
          ( "(cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal))"
          , Atom "equal"
          ),
          ( "(cond ((> 3 3)))"
          , List [Atom ">",RealNumber (LispInteger 3),RealNumber (LispInteger 3)]
          ),
          ( "(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite))"
          , Atom "composite"
          ),
          ( "(case (car '(c d)) ((a e i o u) 'vowel) ((w y) 'semivowel) (else 'consonant)) "
          , Atom "consonant"
          ),
          ( "(make-string 1)"
          , String " "
          ),
          ( "(make-string 3 #\\s)"
          , String "sss"
          ),
          ( "(string #\\s #\\t #\\r #\\i #\\n #\\g)"
          , String "string"
          ),
          ( "(string-length \"Haskell\")"
          , RealNumber (LispInteger 7)
          ),
          ( "(string-length \"\")"
          , RealNumber (LispInteger 0)
          ),
          ( "(string-ref \"Haskell\" 4)"
          , Character 'e'
          ),
          ( "(string=? \"abc\" \"Abc\")"
          , Bool False
          ),
          ( "(string-ci=? \"abc\" \"Abc\")"
          , Bool True
          ),
          ( "(substring \"abcd\" 1 3)"
          , String "bc"
          ),
          ( "(substring \"abcd\" 0 4)"
          , String "abcd"
          ),
          ( "(substring \"abcd\" 0 0)"
          , String ""
          ),
          ( "(string-append \"Hello\" \" \" \"Haskell\" \" world\" \"!\")"
          , String "Hello Haskell world!"
          ),
          ( "(string->list \"abc\")"
          , List [Character 'a', Character 'b', Character 'c']
          ),
          ( "(list->string '(#\\a #\\b #\\c))"
          , String "abc"
          ),
          ( "(string->copy \"abc\")"
          , String "abc"
          )
        ]

  forM_ testCases $ do
    \(input, expected) -> do
      let Right actual = runEval env input in
        assertEqual "" expected actual

badTestEval :: Env -> Test
badTestEval env = TestCase $ do
  let testCases =
        [ "(if 1 (+ 2 3 (- 5 1)) \"unequal\")"
        , "(cond ((< 3 2) 'less) ((< 3 3) 'less))"
        , "(case (car '(c d)) ((a) 'a) ((b) 'b))"
        , "(string-ref \"Haskell\" 8)"
        , "(substring \"abcd\" 0 5)"
        , "(substring \"abcd\" -1 4)"
        , "(substring 4 0 4)"
        ]

  forM_ testCases $ do
    \input -> do
      assert $ isLeft (runEval env input)

runParse :: String -> Either ParseError LispValue
runParse = parse parseExpr "scheme"

runEval :: Env -> String -> ThrowsError LispValue
runEval env input = unsafePerformIO (runExceptT (runEval' env input))

runEval' :: Env -> String -> IOThrowsError LispValue
runEval' env input = case readExpr input of
  Right val -> eval env val
  Left err  -> error $ show err

nullEnv :: IO Env
nullEnv = newIORef []

main :: IO Counts
main = do
  env <- nullEnv
  runTestTT $ TestList [(goodTestParse env), (badTestParse env), (goodTestEval env), (badTestEval env)]
