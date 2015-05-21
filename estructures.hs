{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Estructures where

import qualified Data.Map.Strict as Map
import Data.Time
import Data.Binary
import Control.Monad

type IIMap = Map.Map Int Int  
iiMap = Map.empty :: IIMap

type FTMap = Map.Map FromTo Int
ftMap = Map.empty :: FTMap

-- Answers, containing all answers from the pupils

type Pupil = Int

data Answer = Answer {
   answer_questionId :: Int,
   answer_points :: Int,
   answer_max :: Int
} deriving (Show, Eq)

instance Binary Answer where
   put Answer{..} = do put answer_questionId; put answer_points; put answer_max
   get = do answer_questionId <- get; answer_points <- get; answer_max <- get; return Answer{..}

data Answers = Answers Pupil [Answer] deriving (Eq)

instance Show Answers where
   show (Answers pupil answers) = "Pupil-id: " ++ (show pupil) ++ "\n" ++ (concat $ map (\a -> "  " ++ show a ++ "\n") answers) 

instance Binary Answers where
    put (Answers pupil answers) = do put pupil; put answers;
    get = do pupil <- get; 
	     answers <- get;
             let res = Answers pupil answers 
             return res


--- FromTo questionId
--- Used to connect the different questions
--- Think of it as neurones

data FromTo = FromTo {
   ft_from :: Int,
   ft_to :: Int
} deriving (Eq, Ord)

instance Show FromTo where
   show FromTo{..} = show ft_from ++ "-" ++ show ft_to

instance Binary FromTo where
   put FromTo{..} = do put ft_from; put ft_to;
   get = do ft_from <- get; ft_to <- get; return FromTo{..} 

--- Answer relations
--- Each realation is between a question and all answers that can be connected to that question
--- The base for the connections are found in the answer datatype
--- So if one pupil has answers on question 2 and 4 we calculate the relations between these, based on their points

data Relations = Relations {
   relation_questionId :: Int,
   relation_points :: FTMap, 
   relation_nums :: FTMap,
   relation_max :: FTMap 
} deriving (Show, Eq) 

instance Binary Relations where
   put Relations{..} = do put relation_questionId; put relation_points; put relation_nums; put relation_max;
   get = do relation_questionId <- get; relation_points <- get; relation_nums <- get; relation_max <- get; return Relations{..}

empty_relation qId = Relations qId ftMap ftMap ftMap

--- Answer global results
--- Keeps track of accumulated answers results
--- Each time we get an answer from at pupil we update this structure

data Globals = Globals { 
   globals_points :: IIMap,
   globals_max :: IIMap,
   globals_nums :: IIMap 
} deriving (Show, Eq) 

instance Binary Globals where
   put Globals{..} = do put globals_points; put globals_max; put globals_nums;
   get = do globals_points <- get; globals_max <- get; globals_nums <- get; return Globals{..}

empty_global = Globals iiMap iiMap iiMap

-- A snapshow of learning info on a given point in time

data TimePoint = TimePoint {
   t_year :: Maybe Int,
   t_month :: Maybe Int,
   t_week :: Maybe Int,
   t_relation :: Relations,
   t_globals :: Globals,
   t_answers :: [Answers]
} deriving (Show, Eq)


ccompare v v' n = let res = compare v v' in if res /= EQ then res else n

instance Ord TimePoint where
   (TimePoint year month week _ _ _) `compare` (TimePoint year' month' week' _ _ _) = 
	ccompare year year' $ ccompare month month' $ ccompare week week' EQ 


main2 = do
   putStrLn "Life is short"
   c <- getCurrentTime
   let (y,m,d) = toGregorian $ utctDay c
   print y
