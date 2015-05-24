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
import Calculate

prop_testGlobals = 
   let a1 = Answer 1 1 1
       a2 = Answer 2 2 2 
       globals = empty_global
       as = Answers 10 [a1,a2]
       globals' = addAnswersToGlobals [a1,a2] globals 
       globals'' = addAnswersToGlobals [a1,a2] globals' 
    in globals'' == Globals {globals_points = Map.fromList [(1,2),(2,4)], globals_max = Map.fromList [(1,1),(2,2)], globals_nums = Map.fromList [(1,2),(2,2)]}


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
       Relations questionId points nums = relations''
   r_points <- Map.lookup (FromTo 10 1) points
   r_nums <- Map.lookup (FromTo 10 1) nums
   let res = [r_points, r_nums]
   return res

test_answers pupilId = 
   let a1 = Answer 1 2 3 
       a2 = Answer 2 5 6 
       a3 = Answer 3 3 3
       a4 = Answer 1 1 3
       aList = [a1,a2,a3]
       aList2 = [a4]
       answers = Answers pupilId aList
       answers2 = Answers (pupilId + 1) aList2
       timePoint = empty_timepoint (Just 0) (Just 0) (Just 0)
       timePoint' = addAnswersToTimePoint answers timePoint
       timePoint'' =  addAnswersToTimePoint answers2 timePoint'
    in timePoint''

-- Insert three answers at the same timpoint
-- Ensure that there are thre answers connected to this person after addition
prop_test_answers =
   let TimePoint _ _ _ _ _ answerMap = test_answers 10
       answerList = Map.lookup 10 answerMap
       l = fmap length answerList
    in l == Just 3 
   
prop_test_relation = test_relation == Just [502,2]

-- Insert 4 answers into an timePoint
-- Ensure there are 4 after insertion
test_numberOfAnswers =
   let TimePoint _ _ _ _ _ answerMap = test_answers 10
       n = numberOfAnswers answerMap
    in n

prop_test_numberOfAnswers =
   4 == test_numberOfAnswers  

return []
runTests = $quickCheckAll

