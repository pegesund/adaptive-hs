{-# LANGUAGE TemplateHaskell #-}

module Tests where
import Estructures
import Engine
import qualified Data.Map.Strict as Map
import Test.QuickCheck
import Data.Binary
import Test.QuickCheck.All

prop_testGlobals = 
   let a1 = Answer 1 1 1
       a2 = Answer 2 2 2 
       globals = empty_global
       as = Answers 10 [a1,a2]
       globals' = addResultToGlobals [a1,a2] globals 
       globals'' = addResultToGlobals [a1,a2] globals' 
    in globals'' == Globals {globals_points = Map.fromList [(1,2),(2,4)], globals_max = Map.fromList [(1,2),(2,4)], globals_nums = Map.fromList [(1,2),(2,2)]}


prop_test_binary = do
   let a1 = Answer 1 2 3 
       a2 = Answer 4 5 6 
       as = Answers 10 [a1,a2]
       b1 = encode as
       b2 = decode b1::Answers
    in b2 == as 


return []
runTests = $quickCheckAll

