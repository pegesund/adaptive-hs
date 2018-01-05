module Engine where
import Estructures
import Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import Control.Arrow
import Data.Maybe


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
        (ad_nums g + 1) (ad_pass_points g) (ad_failed g + (if answer_points a >= ad_pass_points g then 1 else 0))
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
      answerPermutations = [(i,answersTo) | i <- answers] ++ [(i,answers) | i <- answersTo] ++ [(i, filter (\a -> i /= a) answersTo) | i <- answersTo]
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

addAnswersToTimePoint :: Answers -> IADMap -> TimePoint -> TimePoint
addAnswersToTimePoint newAnswers answerDataMap timePoint =
   let Answers pupil pupilAnswers = newAnswers
       TimePoint year month week allRelations answerData answers tags = timePoint
       answers' = Map.insertWith (++) pupil pupilAnswers answers
       answerData' = addAnswersToAnswerData pupilAnswers answerData
       allRelations' = addAnswersToAllRelations pupilAnswers pupil answers answerDataMap allRelations 
   in TimePoint year month week allRelations' answerData' answers' tags


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
