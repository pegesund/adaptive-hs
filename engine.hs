module Engine where
import Estructures
import qualified Data.Map.Strict as Map

main = do test_binary


test_globals = do
   let a1 = Answer 1 1 1
       a2 = Answer 2 2 2 
       globals = empty_global
       as = Answers 10 [a1,a2]
       globals' = addResultToGlobals [a1,a2] globals 
       globals'' = addResultToGlobals [a1,a2] globals' 
   print globals''


updateIMap theMap f answers =
   let newNums = map (\a -> (answer_questionId a, f a)) answers
       theMap' = Map.unionWith (+) theMap $ Map.fromList newNums
   in theMap'

addResultToGlobals :: [Answer] -> Globals -> Globals 
addResultToGlobals answers globals =
   let (Globals points max nums) = globals
       nums' = updateIMap nums (\_ -> 1) answers 
       points' = updateIMap points (\a -> answer_points a) answers
       max' = updateIMap points (\a -> answer_max a) answers
       newGlobals = Globals points' max' nums'
   in newGlobals 

addResult newAnswers timePoint = 
   let (Answers pupil pupilAnswers) = newAnswers 
       (TimePoint year month week relation globals answers) = timePoint 
       answers' = answers ++ [newAnswers]
       globals' = addResultToGlobals pupilAnswers globals
   in answers
