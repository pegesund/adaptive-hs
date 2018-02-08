{-# LANGUAGE FlexibleContexts #-}

module Calculate where
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
averageScoreOnQuestion :: IADMap -> Int -> (Double, Double, Double)
averageScoreOnQuestion answerData qid = 
        let global = Map.lookup qid answerData
        in case global of
          Nothing -> error "Trying to calculate on non existing question"
          Just g -> (avg, mx, nums) where
                nums = fromIntegral (ad_nums g)
                avg = ad_points g / nums
                mx = ad_max g / nums

-- Returns a list of (qustionId, pointsScored, score)
-- higher score is better
findPupilsWeakSpots :: IADMap -> IAMap -> Int -> [PupilScore]
findPupilsWeakSpots answerData allAnswers pupilId =
        let maybeAnswers = Map.lookup pupilId allAnswers
          in case maybeAnswers of
                Nothing -> []
                Just answers -> sortedList where
                        oneRes a = let qid = answer_questionId a
                                       points = answer_points a
                                       (score, mx) = let (avg,_mx,_nums) = averageScoreOnQuestion answerData qid in (points/avg, _mx)
                                    in PupilScore qid points mx score
                        l = map oneRes answers
                        sortedList = sort l

smoothPupilWeakSpots :: IADMap -> SmootType -> [PupilScore] -> [PupilScore]
smoothPupilWeakSpots answerData sfactor pupilScores =
        case sfactor of
           SmoothPercentage f -> pupilScore' where
                        sumPoints = Map.foldr (\g acc -> acc + ad_nums g) 0 answerData
                        pupilScore' = filter (hasHigerPrecentage f) pupilScores
                        hasHigerPrecentage _f pupilScore = _f >= 100.0 * getNumGlobal pupilScore / fromIntegral sumPoints 
           SmoothAbsolute f -> filter (hasMoreNums f) pupilScores where
                hasMoreNums _f pupilScore = _f >= getNumGlobal pupilScore 
        where getNumGlobal pupilScore = 
               case Map.lookup (ps_qid pupilScore) answerData of
                 Nothing -> error "Inconsistent data in smoothPupilWeakSpots" 
                 Just g -> fromIntegral $ ad_nums g

xLogX x = if x == 0 then 0 else x * log(x)
entropy2 a b = xLogX(a + b) - xLogX(a) - xLogX(b)
entropy4 a b c d = xLogX(a + b + c + d) - xLogX(a) - xLogX(b) - xLogX(c) - xLogX(d);
logLikelihoodRatio a_and_b a_not_b b_not_a not_a_nor_b =
   let rowEntropy = entropy2 (a_and_b + a_not_b) (b_not_a + not_a_nor_b)
       columnEntropy = entropy2 (a_and_b + b_not_a) (a_not_b + not_a_nor_b)
       matrixEntropy = entropy4 a_and_b a_not_b b_not_a not_a_nor_b
    in if rowEntropy + columnEntropy < matrixEntropy then 0 else 2.0 * (rowEntropy + columnEntropy - matrixEntropy);

