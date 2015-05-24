{-# LANGUAGE TemplateHaskell #-}

module Tests where
import Estructures
import Engine
import qualified Data.Map.Strict as Map
import Test.QuickCheck
import Data.Binary
import Test.QuickCheck.All
import Control.Monad.IO.Class
import Data.Maybe

prop_testGlobals = 
   let a1 = Answer 1 1 1
       a2 = Answer 2 2 2 
       globals = empty_global
       as = Answers 10 [a1,a2]
       globals' = addAnswersToGlobals [a1,a2] globals 
       globals'' = addAnswersToGlobals [a1,a2] globals' 
    in globals'' == Globals {globals_points = Map.fromList [(1,2),(2,4)], globals_max = Map.fromList [(1,2),(2,4)], globals_nums = Map.fromList [(1,2),(2,2)]}


prop_test_binary = do
   let a1 = Answer 1 2 3 
       a2 = Answer 4 5 6 
       as = Answers 10 [a1,a2]
       b1 = encode as
       b2 = decode b1::Answers
    in b2 == as 

test_relation = do
   let a1 = Answer 1 2 3 
       a2 = Answer 4 5 6 
       a3 = Answer 1 500 6 
       as = [a1,a2]
       as2 = [a3]
       relations = empty_relation 10
       relations' = addAnswersToRelations as relations
       relations'' = addAnswersToRelations as2 relations'
       Relations questionId points nums max = relations''
   r_points <- Map.lookup (FromTo 10 1) points
   r_nums <- Map.lookup (FromTo 10 1) nums
   r_max <- Map.lookup (FromTo 10 1) max
   let res = [r_points, r_nums, r_max]
   return res

test_answers = do
   let a1 = Answer 1 2 3 
       a2 = Answer 1 5 6 
       a3 = Answer 2 3 3
       aList = [a1,a2,a3]
       answers = Answers 10 aList
       timePoint = empty_timepoint (Just 0) (Just 0) (Just 0)
       timePoint' = addAnswersToTimePoint answers timePoint 
    in timePoint'

prop_test_answers =
   let TimePoint _ _ _ _ _ answerMap = test_answers
       answerList = Map.lookup 10 answerMap
       l = fmap length answerList
    in l == Just 3 
   
prop_test_relation = test_relation == Just [502,2,9]

return []
runTests = $quickCheckAll

