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

{-# ANN module "HLint: ignore Use camelCase" #-}

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
    testanswerData == fromList [(1,AnswerData {ad_points = 2.0, ad_max = 2.0, ad_nums = 2, ad_pass_points = 2.0, ad_failed = 2}),(2,AnswerData {ad_points = 4.0, ad_max = 2.0, ad_nums = 2, ad_pass_points = 2.0, ad_failed = 0})]


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

testAnswers:: Root
testAnswers =
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
       root'''' = addAnswersToRoot answers2 1 root'''
    in root''''


prop_testAnswers =
   let r = testAnswers in
     r == Root {root_tags = fromList [], root_answerData = fromList [(1,AnswerData {ad_points = 3.0, ad_max = 1.0, ad_nums = 2, ad_pass_points = 1.0, ad_failed = 0}),(2,AnswerData {ad_points = 5.0, ad_max = 1.0, ad_nums = 1, ad_pass_points = 1.0, ad_failed = 0}),(3,AnswerData {ad_points = 3.0, ad_max = 10.0, ad_nums = 1, ad_pass_points = 10.0, ad_failed = 1})], root_courses = fromList [(1,Course {course_all_relations = fromList [(1,Relations {relation_questionId = 1, relation_points = fromList [(2,3),(3,0)], relation_nums = fromList [(2,3),(3,3)]}),(2,Relations {relation_questionId = 2, relation_points = fromList [(1,3),(3,0)], relation_nums = fromList [(1,3),(3,3)]}),(3,Relations {relation_questionId = 3, relation_points = fromList [(1,3),(2,3)], relation_nums = fromList [(1,3),(2,3)]})], course_answers = fromList [(100,[Answer {answer_questionId = 1, answer_points = 2.0},Answer {answer_questionId = 2, answer_points = 5.0},Answer {answer_questionId = 3, answer_points = 3.0}]),(101,[Answer {answer_questionId = 1, answer_points = 1.0}])], course_id = 1, course_total_failed = 1, course_total_passed = 3})]}


-- Insert three answers at the same timpoint
-- Ensure that there are thre answers connected to this person after addition

prop_test_relation::Bool
prop_test_relation = testRelation == Just (2,2)


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


-- Insert some answers into relations
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

testScore misses_a_and_b misses_a misses_b misses_total =
  logLikelihoodRatio misses_a_and_b (misses_a - misses_a_and_b) (misses_b - misses_a_and_b) (misses_total - misses_a - misses_b)


  -- aAndB + aNotB + bNotA + notANorB


return []
runTests::IO Bool
runTests = $quickCheckAll
