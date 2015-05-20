module Engine where
import Estructures
import qualified Data.Map.Strict as Map

main = do test_binary


updateIMap :: IIMap -> (Answer -> Int) -> [Answer] -> IIMap
updateIMap theMap f answers =
   let newNums = map (\a -> (answer_questionId a, f a)) answers
       theMap' = Map.unionWith (+) theMap $ Map.fromList newNums
   in theMap'

addResultToGlobals :: [Answer] -> Globals -> Globals 
addResultToGlobals answers globals =
   let Globals points max nums = globals
       nums' = updateIMap nums (\_ -> 1) answers 
       points' = updateIMap points (\a -> answer_points a) answers
       max' = updateIMap points (\a -> answer_max a) answers
       newGlobals = Globals points' max' nums'
   in newGlobals 

addResult newAnswers timePoint = 
   let Answers pupil pupilAnswers = newAnswers 
       TimePoint year month week relation globals answers = timePoint 
       answers' = answers ++ [newAnswers]
       globals' = addResultToGlobals pupilAnswers globals
   in answers
