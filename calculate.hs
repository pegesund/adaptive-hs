module Calculate where
import Engine
import Estructures
import qualified Data.Map.Strict as Map
import Prelude

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
	    
		 
