module Engine where
import Estructures
import Data.Foldable as Foldable
import qualified Data.Map.Strict as Map


updateRelations :: Num b => Map.Map Int b -> (Answer -> b) -> [Answer] -> p -> Map.Map Int b  
updateRelations theMap f answers _fromId =
   let newNums = map (\a -> (answer_questionId a, f a)) answers
       theMap' = Map.unionWith (+) theMap $ Map.fromList newNums
   in theMap'


updateMap :: Num b => Map.Map Int b -> (Answer -> b) -> [Answer] -> Map.Map Int b
updateMap theMap f answers =
   let newNums = map (\a -> (answer_questionId a, f a)) answers
       theMap' = Map.unionWith (+) theMap $ Map.fromList newNums
   in theMap'

addAnswersToGlobals :: [Answer] -> IGMap -> IGMap
addAnswersToGlobals answers globals =
  let updateGlobal a (Just g) = Globals ((globals_points g) + (answer_points a)) (globals_max g) ((globals_nums g) + 1)
      updateGlobal a Nothing = Globals (answer_points a) 1 1
      f x acc = let qId = answer_questionId x
                    oldVal = Map.lookup qId acc
                    newMap = Map.insert qId (updateGlobal x oldVal) acc
                     in newMap
      in Foldable.foldr' f globals answers 

addAnswersToRelations :: [Answer] -> Relations -> Relations
addAnswersToRelations answers relations =
   let Relations questionId points nums = relations
       points' = updateMap points (\a -> answer_points a) answers
       nums' = updateMap nums (\_ -> 1) answers
   in Relations questionId points' nums'

addAnswersToAllRelations :: [Answer] -> Int -> AllRelations -> IAMap -> AllRelations
addAnswersToAllRelations answers pupilId allRelations allAnswers =
  let pupilAnswers = Map.lookup pupilId allAnswers
      answersTo = case pupilAnswers of
        Just pa -> pa
        Nothing -> []
      answerPermutations = [(i,answersTo) | i <- answers] ++ [(i,answers) | i <- answersTo] ++ [(i, filter (\a -> i /= a) answersTo) | i <- answersTo]
      addToRelation (fromAnswer, toAnswers) acc =
        let fromAnswer_id = answer_questionId fromAnswer
            relations = Map.lookup fromAnswer_id acc
            acc' = case relations of
              Just oldR -> Map.insert fromAnswer_id relations' acc where
                relations' = addAnswersToRelations (filter (\a -> a /= fromAnswer) toAnswers) oldR
              Nothing -> Map.insert fromAnswer_id relations' acc where
                relations' = addAnswersToRelations toAnswers (empty_relation fromAnswer_id)
        in acc'
      in Foldable.foldr' addToRelation allRelations answerPermutations

addAnswersToTimePoint :: Answers -> TimePoint -> TimePoint
addAnswersToTimePoint newAnswers timePoint = 
   let Answers pupil pupilAnswers = newAnswers 
       TimePoint year month week allRelations globals answers tags = timePoint 
       answers' = Map.insertWith (++) pupil pupilAnswers answers
       globals' = addAnswersToGlobals pupilAnswers globals
       allRelations' = addAnswersToAllRelations pupilAnswers pupil allRelations answers
   in TimePoint year month week allRelations' globals' answers' tags


addTagToQuestion :: Tags -> String -> Int -> Tags
addTagToQuestion tags tag qid =
  Map.unionWith addWithoutDuplicates tags $ Map.fromList [(tag, [qid])] where
    addWithoutDuplicates a b = if Prelude.elem (head a) b then b else a ++ b


