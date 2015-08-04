module Engine where
import Estructures
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
      in foldr f globals answers 

addAnswersToRelations :: [Answer] -> Relations -> Relations
addAnswersToRelations answers relations =
   let Relations questionId points nums = relations 
       points' = updateIMap points (\a -> answer_points a) answers
       nums' = updateIMap nums (\_ -> 1) answers
   in Relations questionId points' nums'

addAnswersToTimePoint :: Answers -> TimePoint -> TimePoint
addAnswersToTimePoint newAnswers timePoint = 
   let Answers pupil pupilAnswers = newAnswers 
       TimePoint year month week allRelations globals answers = timePoint 
       answers' = Map.insertWith (++) pupil pupilAnswers answers
       globals' = addAnswersToGlobals pupilAnswers globals
       allRelations' = let oldRelation = Map.lookup  
       -- relation' = addAnswersToRelations pupilAnswers relation
   in TimePoint year month week relation globals' answers' 

   
