module NFA (NFA(..), Move(..), trans) where

import qualified Data.Set as S

data NFA a = NFA
    { states     :: S.Set a,
      moves      :: S.Set (Move a),
      startState :: a,
      finalStates:: S.Set a
    } deriving (Show, Eq)

data Move a = Move a Char a
            | UCMove a a
            | EMove a a
        deriving (Show, Eq, Ord)

toState :: Move a -> a
toState (Move _ _ t) = t
toState (UCMove _ t) = t
toState (EMove _ t) = t

trans :: Ord a => NFA a -> String -> S.Set a
trans nfa = foldl (onetrans nfa) (skipEpsilon nfa (S.singleton (startState nfa)))

onetrans :: Ord a => NFA a -> S.Set a -> Char -> S.Set a
onetrans nfa ss c = skipEpsilon nfa (onemove nfa c ss)

onemove :: Ord a => NFA a -> Char -> S.Set a -> S.Set a
onemove (NFA _ ms _ _) c ss = S.fromList
    [ toState m | s <- S.toList ss,
                  m <- S.toList ms,
                  canMove m s
    ]
    where
        canMove (EMove _ _) _ = False
        canMove (Move fs ch _) s = s == fs && c == ch
        canMove (UCMove fs _) s = s == fs

skipEpsilon :: Ord a => NFA a -> S.Set a -> S.Set a
skipEpsilon nfa@(NFA _ ms _ _) ss = skipMore $ ss `S.union` S.fromList
    [ toState m | s <- S.toList ss,
                  m <- S.toList ms,
                  canMove m s
    ]
    where
        canMove (EMove fs _) s = s == fs
        canMove _ _ = False
        skipMore newStates
            | S.size newStates == S.size ss = newStates
            | otherwise = skipEpsilon nfa newStates
