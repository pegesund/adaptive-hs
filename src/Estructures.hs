{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Estructures where

import qualified Data.Map.Strict as Map
import Data.Time
import Data.Binary

{-# ANN module "HLint: ignore Use camelCase" #-} 


-- Keeps the answeris of each pupil in the form pupilid - answers
type IAMap = Map.Map Int [Answer]
iaMap::IAMap
iaMap = Map.empty


-- Map of integer - integer
type IIMap = Map.Map Int Int
iiMap::IIMap
iiMap = Map.empty


-- Map of integer - double
type IDMap = Map.Map Int Double
idMap::IDMap
idMap = Map.empty

-- Map of questionid - global
-- Global keeps track of invidual statistics for a question
type IGMap = Map.Map Int Globals
igMap::IGMap
igMap = Map.empty


-- Map from questionId to a Relation
type AllRelations = Map.Map Int Relations
newAllRelations::AllRelations
newAllRelations = Map.empty

-- A tag is connected to a question in a many-to-many-relation
-- A tags simpli tags questions

type Tags = Map.Map String [Int]
newTags::Tags
newTags = Map.empty :: Tags

-- Answers, containing all answers from the pupils
-- Every answer contains the max-score, it is for keeping the history of the answer

type Pupil = Int

data Answer = Answer {
   answer_questionId :: Int,
   answer_points :: Double
} deriving (Show, Eq)

instance Binary Answer where
   put Answer{..} = do put answer_questionId; put answer_points
   get = do answer_questionId <- get; answer_points <- get; return Answer{..}

data Answers = Answers Pupil [Answer] deriving (Eq)

instance Show Answers where
   show (Answers pupil answers) = "Pupil-id: " ++ (show pupil) ++ "\n" ++ (concat $ map (\a -> "  " ++ show a ++ "\n") answers) 

instance Binary Answers where
    put (Answers pupil answers) = do put pupil; put answers;
    get = do pupil <- get;
             answers <- get;
             let res = Answers pupil answers
             return res


--- Answer relations
--- Each realation is between a question and all answers that can be connected to that question
--- The base for the connections are found in the answer datatype
--- So if one pupil has answers on question 2 and 4 we calculate the relations between these, based on their points
--- questionId is simpley the QuestionId in the to relation (related to this question)
--- points is the number of aggregated points that is scored in this relations
--- nums as the number of counted relations

data Relations = Relations {
   relation_questionId :: Int,
   relation_points :: IDMap,
   relation_nums :: IIMap
} deriving (Show, Eq)

instance Binary Relations where
   put Relations{..} = do put relation_questionId; put relation_points; put relation_nums;
   get = do relation_questionId <- get; relation_points <- get; relation_nums <- get; return Relations{..}

empty_relation::Int -> Relations
empty_relation qId = Relations qId idMap iiMap

--- Answer global results
--- Keeps track of accumulated answers results
--- Each time we get an answer from at pupil we update this structure
--- points = number of points scored on the question
--- max = max number of point scored on the question
--- numb = number of pupils


data Globals = Globals {
   globals_points :: Double,
   globals_max :: Int,
   globals_nums :: Int
} deriving (Show, Eq)


instance Binary Globals where
   put Globals{..} = do put globals_points; put globals_max; put globals_nums;
   get = do globals_points <- get; globals_max <- get; globals_nums <- get; return Globals{..}


-- A snapshow of learning info on a given point in time

data TimePoint = TimePoint {
   t_year :: Maybe Int,
   t_month :: Maybe Int,
   t_week :: Maybe Int,
   t_all_relations :: AllRelations,
   t_globals :: IGMap,
   t_answers :: IAMap,
   t_tags :: Tags 
} deriving (Show, Eq)

empty_timepoint :: Maybe Int -> Maybe Int -> Maybe Int -> TimePoint
empty_timepoint year month week =
   let t = TimePoint year month week newAllRelations igMap iaMap newTags
   in t

ccompare::Ord p => p -> p -> Ordering -> Ordering
ccompare v v' n = let res = compare v v' in if res /= EQ then res else n

instance Ord TimePoint where
   (TimePoint year month week _ _ _ _) `compare` (TimePoint year' month' week' _ _ _ _) =
      ccompare year year' $ ccompare month month' $ ccompare week week' EQ


-- Score: how good a pupil has scored on a question

data PupilScore = PupilScore {
    ps_qid :: Int,
    ps_points :: Double,
    ps_max :: Double,
    ps_score :: Double
     } deriving (Show, Eq)

instance Ord PupilScore where
  (PupilScore _ _ _ score1) `compare` (PupilScore _ _ _ score2) =
    score1 `compare` score2

-- smoother

data SmootType = SmoothPercentage Double | SmoothAbsolute Double

main2::IO()
main2 = do
   putStrLn "Life is short"
   c <- getCurrentTime
   let (y,_m,_d) = toGregorian $ utctDay c
   print y
