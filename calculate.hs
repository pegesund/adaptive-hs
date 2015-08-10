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

-- Returns avg, max, nums on a queston
averageScoreOnQuestion :: IGMap -> Int -> (Double, Double, Double)
averageScoreOnQuestion globals qid = 
	let global = Map.lookup qid globals
	in case global of
	  Nothing -> error "Trying to calculate on non existing question"
	  Just g -> (avg, mx, nums) where
	  	nums = fromIntegral (globals_nums g)
	  	avg = (fromIntegral (globals_points g)) / nums
	   	mx = (fromIntegral (globals_max g)) / nums
	    
-- Returns a list of (qustionId, pointsScored, score)
-- higher score is better		 
findPupilsWeakSpots :: IGMap -> IAMap -> Int -> [PupilScore]
findPupilsWeakSpots globals allAnswers pupilId =
	let maybeAnswers = Map.lookup pupilId allAnswers
	in case maybeAnswers of
		Nothing -> []
		Just answers -> sortedList where
			oneRes a = let qid = answer_questionId a
			               points = fromIntegral (answer_points a)
			               (score, mx) = let (avg,mx,nums) = averageScoreOnQuestion globals qid in (points/avg, mx)
			            in PupilScore qid points mx score
			l = map oneRes answers
			sortedList = sort l

smoothPupilWeakSpots :: IGMap -> SmootType -> [PupilScore] -> [PupilScore]
smoothPupilWeakSpots globals sfactor pupilScores =
	case sfactor of
	   SmoothPercentage f -> pupilScore' where
	   		sumPoints = Map.foldr (\g acc -> acc + globals_nums g) 0 globals
	   		pupilScore' = filter (hasHigerPrecentage f) pupilScores
	   		hasHigerPrecentage f pupilScore = f >= 100.0 * (getNumGlobal pupilScore) / (fromIntegral sumPoints) 
	   SmoothAbsolute f -> filter (hasMoreNums f) pupilScores where
	        hasMoreNums f pupilScore = f >= getNumGlobal pupilScore 
	where getNumGlobal pupilScore = 
	       case Map.lookup (ps_qid pupilScore) globals of
	         Nothing -> error "Inconsistent data in smoothPupilWeakSpots" 
	         Just g -> fromIntegral $ globals_nums g	    






