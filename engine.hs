module Engine where
import Estructures
import Data.Foldable as Foldable
import qualified Data.Map.Strict as Map


updateRelations :: IIMap -> (Answer -> Int) -> [Answer] -> Int -> IIMap
updateRelations theMap f answers fromId =
   let newNums = map (\a -> (answer_questionId a, f a)) answers
       theMap' = Map.unionWith (+) theMap $ Map.fromList newNums
   in theMap'


updateIMap :: IIMap -> (Answer -> Int) -> [Answer] -> IIMap
updateIMap theMap f answers =
   let newNums = map (\a -> (answer_questionId a, f a)) answers
       theMap' = Map.unionWith (+) theMap $ Map.fromList newNums
   in theMap'

addAnswersToGlobals :: [Answer] -> IGMap -> IGMap 
addAnswersToGlobals answers globals =
  let updateGlobal a (Just g) = Globals ((globals_points g) + (answer_points a)) ((globals_max g) + (answer_max a))  ((globals_nums g) + 1)
      updateGlobal a Nothing = Globals (answer_points a) (answer_max a) 1
      f x acc = let qId = answer_questionId x
                    oldVal = Map.lookup qId acc
                    newMap = Map.insert qId (updateGlobal x oldVal) acc
                     in newMap
      in Foldable.foldr' f globals answers 

addAnswersToRelations :: [Answer] -> Relations -> Relations
addAnswersToRelations answers relations =
   let Relations questionId points nums = relations 
       points' = updateIMap points (\a -> answer_points a) answers
       nums' = updateIMap nums (\_ -> 1) answers
   in Relations questionId points' nums'

addAnswersToAllRelations :: [Answer] -> Int -> AllRelations -> IAMap -> AllRelations
addAnswersToAllRelations answers pupilId allRelations allAnswers =
  let pupilAnswers = Map.lookup pupilId allAnswers
      answersTo = case pupilAnswers of
        Just pa -> pa
        Nothing -> []
      answerPermutations = [(i,answersTo) | i <- answers] ++ [(i,answers) | i <- answersTo]
      addToRelation (fromAnswer, toAnswers) acc = 
        let fromAnswer_id = answer_questionId fromAnswer
            relations = Map.lookup fromAnswer_id acc
            acc' = case relations of
              Just oldR -> Map.insert fromAnswer_id relations' acc where
                relations' = addAnswersToRelations toAnswers oldR
              Nothing -> Map.insert fromAnswer_id relations' acc where
                relations' = addAnswersToRelations toAnswers (empty_relation fromAnswer_id)
        in acc'
      in Foldable.foldr' addToRelation allRelations answerPermutations

addAnswersToTimePoint :: Answers -> TimePoint -> TimePoint
addAnswersToTimePoint newAnswers timePoint = 
   let Answers pupil pupilAnswers = newAnswers 
       TimePoint year month week allRelations globals answers = timePoint 
       answers' = Map.insertWith (++) pupil pupilAnswers answers
       globals' = addAnswersToGlobals pupilAnswers globals
       -- allRelations' = let oldRelation = Map.lookup  
       -- relation' = addAnswersToRelations pupilAnswers relation
   in TimePoint year month week allRelations globals' answers' 

   
