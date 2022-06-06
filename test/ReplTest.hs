import Test.Tasty
import Test.Tasty.HUnit
import Repl

main = defaultMain tests

tests = testGroup "Tests" [
    tokenizeTests,
    parseTests,
    evalTests ]

tokenizeTests = testGroup "Tokenize Tests" [
    testCase "Empty" $ [] @=? tokenize "",
    testCase "No parens" $ tokenize "a 2 CC 3d" @?= ["a", "2", "CC", "3d"],
    testCase "Parens" $ tokenize "(a ((a ((( a b ))) b)) b)" @?= ["(","a","(","(","a","(","(","(","a","b",")",")",")","b",")",")","b",")"]
    ]

parseTests = testGroup "Parse Tests" []

evalTests = testGroup "Eval Tests" []
