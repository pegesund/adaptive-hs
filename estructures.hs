{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Estructures where

import qualified Data.Map.Strict as Map
import Data.Time
import Data.Binary


type IIMap = Map.Map Int Int 
iiMap = Map.empty :: IIMap

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
 
--- Answer relations
--- Each realation is between a question and all answers that can be connected to that question
--- The base for the connections are found in the answer datatype
--- So if one pupil has answers on question 2 and 4 we calculate the relations between these, based on their points

data Relations = Relations {
   realtion_questionId :: Int,
   relation_points :: IIMap, 
   relation_nums :: IIMap,
   relation_max :: IIMap 
} deriving (Show) 

instance Binary Relations where
   put Relations{..} = do put realtion_questionId; put relation_points; put relation_nums; put relation_max;
   get = do realtion_questionId <- get; relation_points <- get; relation_nums <- get; relation_max <- get; return Relations{..}

empty_relation qId = Relations qId iiMap iiMap iiMap

--- Answer global results
--- Keeps track of accumulated answers results
--- Each time we get an answer from at pupil we update this structure

data Globals = Globals { 
   globals_points :: IIMap,
   globals_max :: IIMap,
   globals_nums :: IIMap 
} deriving (Show) 

instance Binary Globals where
   put Globals{..} = do put globals_points; put globals_max; put globals_nums;
   get = do globals_points <- get; globals_max <- get; globals_nums <- get; return Globals{..}

empty_global = Globals iiMap iiMap iiMap

test_binary = do
   let a1 = Answer 1 2 3 
       a2 = Answer 4 5 6 
       as = Answers 10 [a1,a2]
       b1 = encode as
       b2 = decode b1::Answers
   print $ b2 == as 
   print b2
   putStrLn "done"

main2 = do
   putStrLn "Life is short"
   c <- getCurrentTime
   let (y,m,d) = toGregorian $ utctDay c
   print y
