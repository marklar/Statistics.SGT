module Tests where

import Test.HUnit

-- define test cases.
test1 = TestCase (assertEqual "for (foo 3)," (1,2) (2,3))

{--
test2 = TestCase (do (x,y) <- partA 3
                     assertEqual "for the first result of partA," 5 x
                     b <- partB y
                     assertBool ("(partB " ++ show y ++ ") failed") b)
--}

-- name test cases.  group them.
tests = TestList [ TestLabel "test1" test1
                 -- , TestLabel "test2" test2
                 ]
