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

updateFailedGlobal answers root =
   let failedTotal = root_failed_total root
       failedPoint answer acc =
        let answerDataMap = root_answerData root
            adAnswer = case Map.lookup (answer_questionId answer) answerDataMap of
               Just a -> a
               Nothing -> error "Not found answer"
            in acc + (if answer_points answer >= ad_pass_points adAnswer then 1 else 0)
       addPoints = foldr' failedPoint 0 answers
   in root { root_failed_total = (root_failed_total root) + addPoints }

addAnswersToRoot :: Answers -> Int -> Int -> Root -> Root
addAnswersToRoot newAnswers answerId timePointId root =
   let timePoint = case Map.lookup timePointId (root_timePoints root) of
                      Just t -> t
                      Nothing -> error $ "Missing timepointid: " ++ show timePointId
       Answers pupil pupilAnswers = newAnswers
       answers = t_answers timePoint
       answers' = Map.insertWith (++) pupil pupilAnswers answers
       answerData = root_answerData root
       answerData' = addAnswersToAnswerData pupilAnswers answerData
       root' = updateFailedGlobal pupilAnswers root
       allRelations = t_all_relations timePoint
       allRelations' = addAnswersToAllRelations pupilAnswers pupil answers' answerData' allRelations
       timePoint' = timePoint { t_all_relations = allRelations' }
       timePoints' = Map.insert timePointId timePoint' $ root_timePoints root
   in root' { root_timePoints = timePoints', root_answerData = answerData' }


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
