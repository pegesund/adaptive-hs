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

addAnswersToGlobals :: [Answer] -> IGMap -> IGMap 
addAnswersToGlobals answers globals =
  let addGlobals g1 g2 = Globals ((globals_points g1) + (globals_points g2)) ((globals_max g1) + (globals_max g2))  ((globals_nums g1) + (globals_nums g2))
      updateGlobal g (Just oldVal) = addGlobals g oldVal
      updateGlobal g Nothing = g
      f x acc = let qId = answer_questionId x
                    oldVal = Map.lookup qId acc
                    newGlobal = Globals (answer_points x) (answer_max x) 1
                    newMap = Map.insert qId (updateGlobal newGlobal oldVal) acc
                     in newMap
      in foldr f globals answers 

addAnswersToRelations :: [Answer] -> Relations -> Relations
addAnswersToRelations answers relations =
   let Relations questionId points nums = relations 
       points' = updateFTMap points (\a -> answer_points a) answers questionId
       nums' = updateFTMap nums (\_ -> 1) answers questionId
   in Relations questionId points' nums'

addAnswersToTimePoint :: Answers -> TimePoint -> TimePoint
addAnswersToTimePoint newAnswers timePoint = 
   let Answers pupil pupilAnswers = newAnswers 
       TimePoint year month week relation globals answers = timePoint 
       answers' = Map.insertWith (++) pupil pupilAnswers answers
       globals' = addAnswersToGlobals pupilAnswers globals
       relation' = addAnswersToRelations pupilAnswers relation
   in TimePoint year month week relation' globals' answers' 

   
