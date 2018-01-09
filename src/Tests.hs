{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests where
import Estructures
import Engine
import qualified Data.Map.Strict as Map
import Test.QuickCheck
import Data.Binary
import Calculate
import Data.Map.Strict

testanswerData :: IADMap
testanswerData =
   let a1 = Answer 1 1
       a2 = Answer 2 2
       answerData = iadMap
       answerData' = setScore 1 2 2 answerData
       answerData'' = setScore 2 2 2 answerData'
       _as = Answers 10 [a1,a2]
       answerData''' = addAnswersToAnswerData [a1,a2] answerData''
       answerData'''' = addAnswersToAnswerData [a1,a2] answerData'''
    in answerData''''

prop_testanswerData::Bool
prop_testanswerData =
    testanswerData == Map.fromList [(1,AnswerData {ad_points = 2, ad_max = 2, ad_nums = 2, ad_pass_points = 2, ad_failed = 0}),(2,AnswerData {ad_points = 4, ad_max = 2, ad_nums = 2, ad_pass_points = 2, ad_failed = 2})]


prop_test_binary::Bool
prop_test_binary = 
   let a1 = Answer 1 2
       a2 = Answer 4 5
       as = Answers 10 [a1,a2]
       b1 = encode as
       b2 = decode b1::Answers
    in b2 == as

testRelation::Maybe (Int, Int)
testRelation = do
   let a1 = Answer 1 2
       a2 = Answer 4 5
       a3 = Answer 1 500
       answerData = iadMap
       answerData' = setScore 1 1 1 answerData
       answerData'' = setScore 4 1 1 answerData'
       as = [a1,a2]
       as2 = [a3]
       relations = empty_relation 10
       relations' = addAnswersToRelations as answerData'' relations
       relations'' = addAnswersToRelations as2 answerData'' relations'
       Relations _questionId points nums = relations''
   r_points <- Map.lookup 1 points
   r_nums <- Map.lookup 1 nums
   let res = (r_points, r_nums)
   return res

testAnswers::Int -> Root
testAnswers courseId =
   let a1 = Answer 1 2
       a2 = Answer 2 5
       a3 = Answer 3 3
       a4 = Answer 1 1
       pupilId = 100
       answerData = iadMap
       answerData' = setScore 1 1 1 answerData
       answerData'' = setScore 2 1 1 answerData'
       answerData''' = setScore 3 10 10 answerData''
       aList = [a1,a2,a3]
       aList2 = [a4]
       answers = Answers pupilId aList
       answers2 = Answers (pupilId + 1) aList2
       courses = newCourseMap
       root = Root newTags answerData''' courses
       (root', course) = empty_course  root
       courses' = Map.insert 1 course courses
       root'' = root' { root_courses = courses' }
       root''' = addAnswersToRoot answers 1 root''
       root'''' =  addAnswersToRoot answers2 1 root'''
    in root''''

--data Root = Root {
--  root_failed_total :: Int,
--  root_tags :: Tags,
--  root_answerData :: IADMap,
--  root_courses :: TPMap
--}


-- Insert three answers at the same timpoint
-- Ensure that there are thre answers connected to this person after addition

prop_test_relation::Bool
prop_test_relation = testRelation == Just (2,2)

-- Insert 4 answers into an course
-- Ensure there are 4 after insertion
-- testNumberOfAnswers::Int
-- testNumberOfAnswers =
--    let Course _ _ _ _ _ answerMap _ = testAnswers 10
--    in numberOfAnswers answerMap

-- prop_test_numberOfAnswers::Bool
-- prop_test_numberOfAnswers =
--   4 == testNumberOfAnswers

testAllRelations::AllRelations
testAllRelations =
   let allRelations = newAllRelations
       a1 = Answer 1 1
       a2 = Answer 2 1
       a3 = Answer 3 0
       answerData = iadMap
       answerData' = setScore 1 1 1 answerData
       answerData'' = setScore 2 1 1 answerData'
       answerData''' = setScore 3 1 1 answerData''
       aList = [a1,a2]
       aList2 = [a3]
       pupilId = 1
       allAnswers = iaMap
       allAnswers' = Map.insertWith (++) pupilId aList allAnswers
       allRelations' = addAnswersToAllRelations aList2 pupilId  allAnswers' answerData''' allRelations
       allRelations'' = addAnswersToAllRelations aList2 pupilId allAnswers' answerData''' allRelations'
       allRelations''' = addAnswersToAllRelations aList2 pupilId allAnswers' answerData''' allRelations''
       allRelations'''' = addAnswersToAllRelations aList2 pupilId allAnswers' answerData''' allRelations'''
    in allRelations''''


-- Insert some ansers into relations
-- Make sure binding are counted correct
-- For example should the binding-value between answer-1 and answer-2 increase if they was solved toghter
-- What is caught in the relation is number of points, and the possible max-points
prop_tescourse_all_relations::Bool
prop_tescourse_all_relations =
  let r = testAllRelations
      three = Map.lookup 1 r
  in three == Just Relations {relation_questionId = 1, relation_points = fromList [(2,4),(3,0)], relation_nums = fromList [(2,4),(3,4)]}

prop_test_setScore::Bool
prop_test_setScore =
  let answerData = iadMap
      answerData' = setScore 1 10.5 5 answerData
      answerData'' = setScore 1 11.5 6 answerData
  in answerData' == fromList [(1,AnswerData {ad_points = 0.0, ad_max = 10.5, ad_nums = 0, ad_pass_points  = 5, ad_failed = 0})] &&
     answerData''== fromList [(1,AnswerData {ad_points = 0.0, ad_max = 11.5, ad_nums = 0, ad_pass_points = 6, ad_failed = 0})]

return []
runTests::IO Bool
runTests = $quickCheckAll
