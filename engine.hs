module Engine where
import Estructures
import qualified Data.Map.Strict as Map


updateFTMap :: FTMap -> (Answer -> Int) -> [Answer] -> Int -> FTMap
updateFTMap theMap f answers fromId =
   let newNums = map (\a -> (FromTo fromId $ answer_questionId a, f a)) answers
       theMap' = Map.unionWith (+) theMap $ Map.fromList newNums
   in theMap'


updateIMap :: IIMap -> (Answer -> Int) -> [Answer] -> IIMap
updateIMap theMap f answers =
   let newNums = map (\a -> (answer_questionId a, f a)) answers
       theMap' = Map.unionWith (+) theMap $ Map.fromList newNums
   in theMap'

addAnswersToGlobals :: [Answer] -> Globals -> Globals 
addAnswersToGlobals answers globals =
   let Globals points max nums = globals
       nums' = updateIMap nums (\_ -> 1) answers 
       points' = updateIMap points (\a -> answer_points a) answers
       max' = updateIMap points (\a -> answer_max a) answers
       newGlobals = Globals points' max' nums'
   in newGlobals 

addAnswersToRelations :: [Answer] -> Relations -> Relations
addAnswersToRelations answers relations =
   let Relations questionId points nums max = relations 
       points' = updateFTMap points (\a -> answer_points a) answers questionId
       nums' = updateFTMap nums (\_ -> 1) answers questionId
       max' = updateFTMap max (\a -> answer_max a) answers questionId
   in Relations questionId points' nums' max' 

addResult newAnswers timePoint = 
   let Answers pupil pupilAnswers = newAnswers 
       TimePoint year month week relation globals answers = timePoint 
       answers' = answers ++ [newAnswers]
       globals' = addAnswersToGlobals pupilAnswers globals
   in answers
