module Calculate where
import Engine
import Estructures
import qualified Data.Map.Strict as Map
import Prelude
import Data.List

numberOfAnswers :: IAMap -> Int
numberOfAnswers answerMap =
   let f answers acc = acc + length answers
    in Map.foldr' f 0 answerMap

answersFromPupil :: IAMap -> Int -> Maybe [Answer]
answersFromPupil allAnswers pupilId =
   let answers = Map.lookup pupilId allAnswers
     in answers

-- Returns avg and max score on a queston
averageScoreOnQuestion :: IGMap -> Int -> (Double, Double)
averageScoreOnQuestion globals qid = 
	let global = Map.lookup qid globals
	in case global of
	  Nothing -> error "Trying to calculate on non existing question"
	  Just g -> (avg, mx) where
	  	avg = (fromIntegral (globals_points g)) / fromIntegral (globals_nums g) 
	   	mx = (fromIntegral (globals_max g)) / fromIntegral (globals_nums g)
	    
-- Returns a list of (qustionId, pointsScored, score)
-- higher score is better		 
findPupilsWeakSpots :: IGMap -> IAMap -> Int -> [(Int, Double, Double)]
findPupilsWeakSpots globals allAnswers pupilId =
	let maybeAnswers = Map.lookup pupilId allAnswers
	in case maybeAnswers of
		Nothing -> []
		Just answers -> sortedList where
			oneRes a = let qid = answer_questionId a
			               points = fromIntegral (answer_points a)
			               score = let (avg,mx) = averageScoreOnQuestion globals qid in points/avg
			            in (qid,points,score)
			l = map oneRes answers
			sortedList = sortBy (\(q1,p1,a1) (q2,p2,a2) -> a1 `compare` a2) l