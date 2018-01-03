{-# LANGUAGE TemplateHaskell #-}

module Tests where
import Estructures
import Engine
import qualified Data.Map.Strict as Map
import Test.QuickCheck
import Data.Binary
import Calculate
import Data.Map.Strict

prop_testGlobals::Bool
prop_testGlobals =
   let a1 = Answer 1 1 1
       a2 = Answer 2 2 2
       globals = igMap
       _as = Answers 10 [a1,a2]
       globals' = addAnswersToGlobals [a1,a2] globals 
       globals'' = addAnswersToGlobals [a1,a2] globals' 
    in globals'' == Map.fromList [(1,Globals {globals_points = 2, globals_max = 2, globals_nums = 2}),(2,Globals {globals_points = 4, globals_max = 4, globals_nums = 2})]

prop_test_binary::Bool
prop_test_binary = do
   let a1 = Answer 1 2 3
       a2 = Answer 4 5 6
       as = Answers 10 [a1,a2]
       b1 = encode as
       b2 = decode b1::Answers
    in b2 == as

test_relation::Maybe [Int]
test_relation = do
   let a1 = Answer 1 2 3
       a2 = Answer 4 5 6
       a3 = Answer 1 500 6
       as = [a1,a2]
       as2 = [a3]
       relations = empty_relation 10
       relations' = addAnswersToRelations as relations
       relations'' = addAnswersToRelations as2 relations'
       Relations _questionId points nums = relations''
   r_points <- Map.lookup 1 points
   r_nums <- Map.lookup 1 nums
   let res = [r_points, r_nums]
   return res

test_answers::Int -> TimePoint
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
prop_test_answers::Bool
prop_test_answers =
   let TimePoint _ _ _ _ _ answerMap _ = test_answers 10
       answerList = Map.lookup 10 answerMap
       l = fmap length answerList
    in l == Just 3

prop_test_relation::Bool
prop_test_relation = test_relation == Just [502,2]

-- Insert 4 answers into an timePoint
-- Ensure there are 4 after insertion
test_numberOfAnswers::Int
test_numberOfAnswers =
   let TimePoint _ _ _ _ _ answerMap _ = test_answers 10
    in numberOfAnswers answerMap

prop_test_numberOfAnswers::Bool
prop_test_numberOfAnswers =
   4 == test_numberOfAnswers

test_all_relations::AllRelations
test_all_relations =
   let allRelations = newAllRelations
       a1 = Answer 1 1 30
       a2 = Answer 2 1 20 
       a3 = Answer 3 1 10
       aList = [a1,a2]
       aList2 = [a3]
       pupilId = 1
       allAnswers = iaMap
       allAnswers' = Map.insertWith (++) pupilId aList allAnswers
       allRelations' = addAnswersToAllRelations aList2 pupilId allRelations allAnswers'
       allRelations'' = addAnswersToAllRelations aList2 pupilId allRelations' allAnswers'
       allRelations''' = addAnswersToAllRelations aList2 pupilId allRelations'' allAnswers'
       allRelations'''' = addAnswersToAllRelations aList2 pupilId allRelations''' allAnswers'
    in allRelations''''


-- Insert some ansers into relations
-- Make sure binding are counted correct
-- For example should the binding-value between answer-1 and answer-2 increase if they was solved toghter
-- What is caught in the relation is number of points, and the possible max-points
prop_test_all_relations::Bool
prop_test_all_relations =
  let r = test_all_relations
      three = Map.lookup 3 r
  in three == Just (Relations {relation_questionId = 3, relation_points = fromList [(1,4),(2,4)], relation_nums = fromList [(1,4),(2,4)]})

return []
runTests::IO Bool
runTests = $quickCheckAll

