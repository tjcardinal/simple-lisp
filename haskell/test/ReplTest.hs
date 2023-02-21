import Repl
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Repl Tests"
    [ parseTests,
      additionTests,
      subtractionTests,
      multiplicationTests,
      divisionTests,
      greaterThanTests,
      greaterThanOrEqualToTests,
      lessThanTests,
      lessThanOrEqualToTests,
      equalityTests,
      ifTests,
      defTests,
      lambdaTests
    ]

parseTests :: TestTree
parseTests =
  testGroup
    "Parse Tests"
    [ checkEqual "" $ Left $ ParseError EmptyTokenList,
      checkEqual "()" $ Right (List [], defaultEnv),
      checkEqual ")" $ Left $ ParseError InvalidRightParens,
      checkEqual "(+ 2 3" $ Left $ ParseError MissingRightParens,
      checkEqual "(+ 2 3) 4" $ Left $ ParseError TokensRemaining,
      checkEqual "(+ 2 (- 3 (* 4 (/ 5 6))))" $ Right (Number $ 1 + 2 / 3, defaultEnv)
    ]

additionTests :: TestTree
additionTests =
  testGroup
    "Addition Tests"
    [ checkEqual "(+)" $ Right (Number $ 0, defaultEnv),
      checkEqual "(+ 2)" $ Right (Number $ 2, defaultEnv),
      checkEqual "(+ 2 3)" $ Right (Number $ 5, defaultEnv),
      checkEqual "(+ 2 3 4)" $ Right (Number $ 9, defaultEnv),
      checkEqual "(+ -2)" $ Right (Number $ -2, defaultEnv),
      checkEqual "(+ 2 -3)" $ Right (Number $ -1, defaultEnv),
      checkEqual "(+ a 1)" $ Left $ ExprError UnknownSymbol,
      checkEqual "(+ 1 a)" $ Left $ ExprError UnknownSymbol,
      checkEqual "(+ true 1)" $ Left $ ExprError NotANumber,
      checkEqual "(+ 1 true)" $ Left $ ExprError NotANumber
    ]

subtractionTests :: TestTree
subtractionTests =
  testGroup
    "Subtraction Tests"
    [ checkEqual "(-)" $ Right (Number $ 0, defaultEnv),
      checkEqual "(- 2)" $ Right (Number $ -2, defaultEnv),
      checkEqual "(- 2 3)" $ Right (Number $ -1, defaultEnv),
      checkEqual "(- 2 3 4)" $ Right (Number $ -5, defaultEnv),
      checkEqual "(- -2)" $ Right (Number $ 2, defaultEnv),
      checkEqual "(- 2 -3)" $ Right (Number $ 5, defaultEnv),
      checkEqual "(- a 1)" $ Left $ ExprError UnknownSymbol,
      checkEqual "(- 1 a)" $ Left $ ExprError UnknownSymbol,
      checkEqual "(- true 1)" $ Left $ ExprError NotANumber,
      checkEqual "(- 1 true)" $ Left $ ExprError NotANumber
    ]

multiplicationTests :: TestTree
multiplicationTests =
  testGroup
    "Multiplication Tests"
    [ checkEqual "(*)" $ Right (Number $ 1, defaultEnv),
      checkEqual "(* 2)" $ Right (Number $ 2, defaultEnv),
      checkEqual "(* 2 3)" $ Right (Number $ 6, defaultEnv),
      checkEqual "(* 2 3 4)" $ Right (Number $ 24, defaultEnv),
      checkEqual "(* -2)" $ Right (Number $ -2, defaultEnv),
      checkEqual "(* 2 -3)" $ Right (Number $ -6, defaultEnv),
      checkEqual "(* a 1)" $ Left $ ExprError UnknownSymbol,
      checkEqual "(* 1 a)" $ Left $ ExprError UnknownSymbol,
      checkEqual "(* true 1)" $ Left $ ExprError NotANumber,
      checkEqual "(* 1 true)" $ Left $ ExprError NotANumber
    ]

divisionTests :: TestTree
divisionTests =
  testGroup
    "Division Tests"
    [ checkEqual "(/)" $ Right (Number $ 1, defaultEnv),
      checkEqual "(/ 2)" $ Right (Number $ 1 / 2, defaultEnv),
      checkEqual "(/ 2 3)" $ Right (Number $ 2 / 3, defaultEnv),
      checkEqual "(/ 2 3 4)" $ Right (Number $ 2 / 3 / 4, defaultEnv),
      checkEqual "(/ -2)" $ Right (Number $ -1 / 2, defaultEnv),
      checkEqual "(/ 2 -3)" $ Right (Number $ -2 / 3, defaultEnv),
      checkEqual "(/ a 1)" $ Left $ ExprError UnknownSymbol,
      checkEqual "(/ 1 a)" $ Left $ ExprError UnknownSymbol,
      checkEqual "(/ true 1)" $ Left $ ExprError NotANumber,
      checkEqual "(/ 1 true)" $ Left $ ExprError NotANumber
    ]

greaterThanTests :: TestTree
greaterThanTests =
  testGroup
    "Greater than Tests"
    [ checkEqual "(>)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(> 2)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(> 2 2)" $ Right (Boolean $ False, defaultEnv),
      checkEqual "(> 2 3)" $ Right (Boolean $ False, defaultEnv),
      checkEqual "(> 3 2)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(> 2 2 2)" $ Right (Boolean $ False, defaultEnv),
      checkEqual "(> 2 3 4)" $ Right (Boolean $ False, defaultEnv),
      checkEqual "(> 4 3 2)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(> -2)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(> 2 -3)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(> a 1)" $ Left $ ExprError UnknownSymbol,
      checkEqual "(> 1 a)" $ Left $ ExprError UnknownSymbol,
      checkEqual "(> true 1)" $ Left $ ExprError NotANumber,
      checkEqual "(> 1 true)" $ Left $ ExprError NotANumber
    ]

greaterThanOrEqualToTests :: TestTree
greaterThanOrEqualToTests =
  testGroup
    "Greater than or equal to Tests"
    [ checkEqual "(>=)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(>= 2)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(>= 2 2)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(>= 2 3)" $ Right (Boolean $ False, defaultEnv),
      checkEqual "(>= 3 2)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(>= 2 2 2)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(>= 2 3 4)" $ Right (Boolean $ False, defaultEnv),
      checkEqual "(>= 4 3 2)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(>= -2)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(>= 2 -3)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(>= a 1)" $ Left $ ExprError UnknownSymbol,
      checkEqual "(>= 1 a)" $ Left $ ExprError UnknownSymbol,
      checkEqual "(>= true 1)" $ Left $ ExprError NotANumber,
      checkEqual "(>= 1 true)" $ Left $ ExprError NotANumber
    ]

lessThanTests :: TestTree
lessThanTests =
  testGroup
    "Less than Tests"
    [ checkEqual "(<)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(< 2)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(< 2 2)" $ Right (Boolean $ False, defaultEnv),
      checkEqual "(< 2 3)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(< 3 2)" $ Right (Boolean $ False, defaultEnv),
      checkEqual "(< 2 2 2)" $ Right (Boolean $ False, defaultEnv),
      checkEqual "(< 2 3 4)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(< 4 3 2)" $ Right (Boolean $ False, defaultEnv),
      checkEqual "(< -2)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(< 2 -3)" $ Right (Boolean $ False, defaultEnv),
      checkEqual "(< a 1)" $ Left $ ExprError UnknownSymbol,
      checkEqual "(< 1 a)" $ Left $ ExprError UnknownSymbol,
      checkEqual "(< true 1)" $ Left $ ExprError NotANumber,
      checkEqual "(< 1 true)" $ Left $ ExprError NotANumber
    ]

lessThanOrEqualToTests :: TestTree
lessThanOrEqualToTests =
  testGroup
    "Less than or equal to Tests"
    [ checkEqual "(<=)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(<= 2)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(<= 2 2)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(<= 2 3)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(<= 3 2)" $ Right (Boolean $ False, defaultEnv),
      checkEqual "(<= 2 2 2)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(<= 2 3 4)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(<= 4 3 2)" $ Right (Boolean $ False, defaultEnv),
      checkEqual "(<= -2)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(<= 2 -3)" $ Right (Boolean $ False, defaultEnv),
      checkEqual "(<= a 1)" $ Left $ ExprError UnknownSymbol,
      checkEqual "(<= 1 a)" $ Left $ ExprError UnknownSymbol,
      checkEqual "(<= true 1)" $ Left $ ExprError NotANumber,
      checkEqual "(<= 1 true)" $ Left $ ExprError NotANumber
    ]

equalityTests :: TestTree
equalityTests =
  testGroup
    "Equality Tests"
    [ checkEqual "(=)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(= 2)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(= 2 2)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(= 2 3)" $ Right (Boolean $ False, defaultEnv),
      checkEqual "(= 2 2 2)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(= 2 3 4)" $ Right (Boolean $ False, defaultEnv),
      checkEqual "(= -2)" $ Right (Boolean $ True, defaultEnv),
      checkEqual "(= 2 -3)" $ Right (Boolean $ False, defaultEnv),
      checkEqual "(= a 1)" $ Left $ ExprError UnknownSymbol,
      checkEqual "(= 1 a)" $ Left $ ExprError UnknownSymbol,
      checkEqual "(= true 1)" $ Left $ ExprError NotANumber,
      checkEqual "(= 1 true)" $ Left $ ExprError NotANumber
    ]

ifTests :: TestTree
ifTests =
  testGroup
    "If Tests"
    [ checkEqual "(if)" $ Left $ ExprError InvalidForm,
      checkEqual "(if true)" $ Left $ ExprError InvalidForm,
      checkEqual "(if true 1)" $ Left $ ExprError InvalidForm,
      checkEqual "(if true 1 2)" $ Right (Number $ 1, defaultEnv),
      checkEqual "(if false)" $ Left $ ExprError InvalidForm,
      checkEqual "(if false 1)" $ Left $ ExprError InvalidForm,
      checkEqual "(if false 1 2)" $ Right (Number $ 2, defaultEnv),
      checkEqual "(if a 1 2)" $ Left $ ExprError UnknownSymbol,
      checkEqual "(if 1 2 3)" $ Left $ ExprError InvalidForm
    ]

defTests :: TestTree
defTests =
  testGroup
    "Def Tests"
    [ ( case parseAndEval defaultEnv "(def a 2)" of
          Right (_, env) -> checkDef env "a" $ Right (Number $ 2, env)
          _ -> testCase "" $ assertFailure "def failed"
      ),
      ( case parseAndEval defaultEnv "(def a true)" of
          Right (_, env) -> checkDef env "a" $ Right (Boolean $ True, env)
          _ -> testCase "" $ assertFailure "def failed"
      ),
      ( case parseAndEval defaultEnv "(def a (+ 2 3))" of
          Right (_, env) -> checkDef env "a" $ Right (Number $ 5, env)
          _ -> testCase "" $ assertFailure "def failed"
      ),
      ( case parseAndEval defaultEnv "(def a +)" of
          Right (_, env) -> checkDef env "(a 2 3)" $ Right (Number $ 5, env)
          _ -> testCase "" $ assertFailure "def failed"
      ),
      ( case parseAndEval defaultEnv "(def a (lambda (a b) (+ a b)))" of
          Right (_, env) -> checkDef env "(a 2 3)" $ Right (Number $ 5, env)
          _ -> testCase "" $ assertFailure "def failed"
      )
    ]

lambdaTests :: TestTree
lambdaTests =
  testGroup
    "Lambda Tests"
    [ checkEqual "((lambda (a b) (+ a b)) 2 3)" $ Right (Number 5, defaultEnv),
      checkEqual "((lambda (a b c) (+ a b)) 2 3 4)" $ Right (Number 5, defaultEnv),
      checkEqual "((lambda (a b c) (+ a b)) 2 3)" $ Left $ ExprError InvalidForm,
      checkEqual "((lambda (a b) (+ a b c)) 2 3)" $ Left $ ExprError UnknownSymbol,
      checkEqual "((lambda (a b) (+ a b)) 2 3 4)" $ Left $ ExprError InvalidForm,
      checkEqual "((lambda a b (+ a b)) 2 3)" $ Left $ ExprError InvalidForm,
      checkEqual "((lambda (a b) + a b c) 2 3)" $ Left $ ExprError InvalidForm
    ]

checkEqual :: String -> Either ReplError (Expr, Env) -> TestTree
checkEqual input expected = testCase input $ parseAndEval defaultEnv input @?= expected

checkDef :: Env -> String -> Either ReplError (Expr, Env) -> TestTree
checkDef env input output = testCase input $ parseAndEval env input @?= output