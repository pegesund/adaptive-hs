{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict, StrictData #-}
module Engine where
import Estructures
import Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import Control.Arrow
import Data.Maybe

{-# ANN module "HLint: ignore Use fromMaybe" #-}


updateRelations :: Num b => Map.Map Int b -> (Answer -> b) -> [Answer] -> p -> Map.Map Int b
updateRelations theMap f answers _fromId =
   let newNums = map (answer_questionId &&& f) answers
       theMap' = Map.unionWith (+) theMap $ Map.fromList newNums
   in theMap'


updateMap :: Num b =>  (Answer -> b) -> [Answer] -> Map.Map Int b -> Map.Map Int b
updateMap f answers theMap =
   let newNums = map (answer_questionId &&& f) answers
       theMap' = Map.unionWith (+) theMap $ Map.fromList newNums
   in theMap'

addAnswersToAnswerData :: [Answer] -> IADMap -> IADMap
addAnswersToAnswerData answers answerData =
  let updateGlobal a (Just g) = AnswerData (ad_points g + answer_points a) (ad_max g)
                                           (ad_nums g + 1) (ad_pass_points g) (ad_failed g + (if answer_points a < ad_pass_points g then 1 else 0))
      updateGlobal a Nothing = error "Question not created"
      f x acc = let qId = answer_questionId x
                    oldVal = Map.lookup qId acc
                    newMap = Map.insert qId (updateGlobal x oldVal) acc
                     in newMap
      in Foldable.foldr' f answerData answers

addAnswersToRelations :: [Answer] -> IADMap -> Relations -> Relations
addAnswersToRelations answers answerDataMap relations =
   let Relations questionId points nums = relations
       calc_points a = let answerData = Map.lookup (answer_questionId a) answerDataMap in
         case answerData of
           Just (AnswerData _ _ _ pass _) -> if answer_points a >= pass then 1 else 0
           Nothing -> error "Question not found"
       points' = updateMap calc_points answers points
       nums' = updateMap (const 1) answers nums
   in Relations questionId points' nums'

addAnswersToAllRelations :: [Answer] -> Pupil -> IAMap -> IADMap -> AllRelations -> AllRelations
addAnswersToAllRelations answers pupilId allAnswers answerDataMap allRelations =
  let pupilAnswers = Map.lookup pupilId allAnswers
      answersTo = fromMaybe [] pupilAnswers
      answerPermutations = [(i,answersTo) | i <- answers] ++ [(i,answers) | i <- answersTo] ++ [(i, filter (/= i) answersTo) | i <- answersTo]
      addToRelation (fromAnswer, toAnswers) acc =
        let fromAnswer_id = answer_questionId fromAnswer
            relations = Map.lookup fromAnswer_id acc
            acc' = case relations of
              Just oldR -> Map.insert fromAnswer_id relations' acc where
                relations' = addAnswersToRelations (filter (/= fromAnswer) toAnswers) answerDataMap oldR
              Nothing -> Map.insert fromAnswer_id relations' acc where
                relations' = addAnswersToRelations toAnswers answerDataMap (empty_relation fromAnswer_id)
        in acc'
      in Foldable.foldr' addToRelation allRelations answerPermutations


updateFailedAndPassed :: [Answer] -> Int -> Root -> Root
updateFailedAndPassed answers courseId root =
   let failedPoint answer acc =
         let answerDataMap = root_answerData root
             adAnswer = case Map.lookup (answer_questionId answer) answerDataMap of
               Just a -> a
               Nothing -> error "Not found answer"
            in acc + (if answer_points answer < ad_pass_points adAnswer then 1 else 0)
       addPointsFailed = foldr' failedPoint 0 answers
       course = case Map.lookup courseId (root_courses root) of
                  Just c -> c
                  Nothing -> error $ "Not found course id: " ++ show courseId
       course' = course { course_total_failed = course_total_failed course + addPointsFailed,
                          course_total_passed = course_total_passed course + (length answers - addPointsFailed)
                          }
   in root { root_courses = Map.insert courseId course' (root_courses root) }

addAnswersToRoot :: Answers -> Int -> Root -> Root
addAnswersToRoot newAnswers courseId root =
   let Answers pupil pupilAnswers = newAnswers
       root' = updateFailedAndPassed pupilAnswers courseId root
       course = case Map.lookup courseId (root_courses root') of
                      Just t -> t
                      Nothing -> error $ "Missing courseid: " ++ show courseId
       answers = course_answers course
       answers' = Map.insertWith (++) pupil pupilAnswers answers
       answerData = root_answerData root
       answerData' = addAnswersToAnswerData pupilAnswers answerData
       allRelations = course_all_relations course
       allRelations' = addAnswersToAllRelations pupilAnswers pupil answers' answerData' allRelations
       newPupilAnswers = case Map.lookup pupil (course_answers course) of
                            Nothing -> pupilAnswers
                            Just a -> pupilAnswers ++ a
       newAllAnswers = Map.insert pupil newPupilAnswers (course_answers course)
       course' = course { course_all_relations = allRelations', course_answers = newAllAnswers }
       courses' = Map.insert courseId course' $ root_courses root'
   in root' { root_courses = courses', root_answerData = answerData' }


addTagToQuestion :: Tags -> String -> Int -> Tags
addTagToQuestion tags tag qid =
  Map.unionWith addWithoutDuplicates tags $ Map.fromList [(tag, [qid])] where
    addWithoutDuplicates a b = if head a `elem` b then b else a ++ b

setScore :: Int -> Double -> Double -> IADMap -> IADMap
setScore questionId maxVal passPoint answerData =
   let  oldGlobal =  Map.lookup questionId answerData
        newGlobal = case oldGlobal of
           Just (AnswerData points _max nums _pass_point failed) -> AnswerData points maxVal nums passPoint failed
           Nothing -> AnswerData 0 maxVal 0 passPoint 0
   in Map.insert questionId newGlobal answerData
