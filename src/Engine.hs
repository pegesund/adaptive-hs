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

addAnswersToGlobals :: [Answer] -> IGMap -> IGMap
addAnswersToGlobals answers globals =
  let updateGlobal a (Just g) = Globals (globals_points g + answer_points a) (globals_max g)
        (globals_nums g + 1) (globals_pass_points g)
      updateGlobal a Nothing = Globals (answer_points a) 1 1 1
      f x acc = let qId = answer_questionId x
                    oldVal = Map.lookup qId acc
                    newMap = Map.insert qId (updateGlobal x oldVal) acc
                     in newMap
      in Foldable.foldr' f globals answers

addAnswersToRelations :: [Answer] -> IGMap -> Relations -> Relations
addAnswersToRelations answers globalsMap relations =
   let Relations questionId points nums = relations
       calc_points a = let globals = Map.lookup (answer_questionId a) globalsMap in
         case globals of
           Just (Globals _ _ _ pass ) -> if (answer_points a) >= pass then 1 else 0
           Nothing -> error "Question not found"
       points' = updateMap calc_points answers points
       nums' = updateMap (const 1) answers nums
   in Relations questionId points' nums'

addAnswersToAllRelations :: [Answer] -> Pupil -> IAMap -> IGMap -> AllRelations -> AllRelations
addAnswersToAllRelations answers pupilId allAnswers globalsMap allRelations =
  let pupilAnswers = Map.lookup pupilId allAnswers
      answersTo = fromMaybe [] pupilAnswers
      answerPermutations = [(i,answersTo) | i <- answers] ++ [(i,answers) | i <- answersTo] ++ [(i, filter (\a -> i /= a) answersTo) | i <- answersTo]
      addToRelation (fromAnswer, toAnswers) acc =
        let fromAnswer_id = answer_questionId fromAnswer
            relations = Map.lookup fromAnswer_id acc
            acc' = case relations of
              Just oldR -> Map.insert fromAnswer_id relations' acc where
                relations' = addAnswersToRelations (filter (/= fromAnswer) toAnswers) globalsMap oldR
              Nothing -> Map.insert fromAnswer_id relations' acc where
                relations' = addAnswersToRelations toAnswers globalsMap (empty_relation fromAnswer_id)
        in acc'
      in Foldable.foldr' addToRelation allRelations answerPermutations

addAnswersToTimePoint :: Answers -> IGMap -> TimePoint -> TimePoint
addAnswersToTimePoint newAnswers globalsMap timePoint =
   let Answers pupil pupilAnswers = newAnswers
       TimePoint year month week allRelations globals answers tags = timePoint
       answers' = Map.insertWith (++) pupil pupilAnswers answers
       globals' = addAnswersToGlobals pupilAnswers globals
       allRelations' = addAnswersToAllRelations pupilAnswers pupil answers globalsMap allRelations 
   in TimePoint year month week allRelations' globals' answers' tags


addTagToQuestion :: Tags -> String -> Int -> Tags
addTagToQuestion tags tag qid =
  Map.unionWith addWithoutDuplicates tags $ Map.fromList [(tag, [qid])] where
    addWithoutDuplicates a b = if head a `elem` b then b else a ++ b

setScore :: Int -> Double -> Double -> IGMap -> IGMap
setScore questionId maxVal passPoint globals =
   let  oldGlobal =  Map.lookup questionId globals
        newGlobal = case oldGlobal of
           Just (Globals points _max nums _pass_point) -> Globals points maxVal nums passPoint
           Nothing -> Globals 0 maxVal 0 passPoint 
   in Map.insert questionId newGlobal globals
