module Calculate where
import Engine
import Estructures
import qualified Data.Map.Strict as Map

numberOfAnswers :: IAMap -> Int
numberOfAnswers answerMap =
   let f answers acc = acc + length answers
    in Map.foldr' f 0 answerMap

answersFromPupilInTimePoint :: IAMap -> Int -> Maybe [Answer]
answersFromPupilInTimePoint allAnswers pupilId =
   let answers = Map.lookup pupilId allAnswers
     in answers


