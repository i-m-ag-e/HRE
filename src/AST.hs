module AST(RExpr(..), build) where 

import NFA(NFA(..), Move(..))
import qualified Data.Set as S

data RExpr = Empty
          | Wildcard
          | Symbol Char
          | Union RExpr RExpr
          | Concat RExpr RExpr
          | Star RExpr
          | Plus RExpr
          | Option RExpr
          deriving (Show, Eq)

build :: RExpr -> NFA Int
build (Symbol c) = NFA {
        states = S.fromAscList [0, 1],
        moves = S.fromList [Move 0 c 1],
        startState = 0,
        finalStates = S.fromAscList [1]
    }

build Wildcard = NFA {
        states = S.fromAscList [0, 1],
        moves = S.fromList [UCMove 0 1],
        startState = 0,
        finalStates = S.fromAscList [1]
    }

build Empty = error "Cannot build NFA for empty regex"

build (Union l r) = makeAlt (build l) (build r)
build (Concat l r) = makeConcat (build l) (build r)
build rep@(Star r) = makeRep (build r) rep
build rep@(Plus r) = makeRep (build r) rep
build rep@(Option r) = makeRep (build r) rep

renumberMoves :: S.Set (Move Int) -> Int -> S.Set (Move Int)
renumberMoves ms offset = S.map renumberMove ms
    where
        renumberMove (Move s v e) = Move (s + offset) v (e + offset)
        renumberMove (UCMove s e) = UCMove (s + offset) (e + offset)
        renumberMove (EMove s e) = EMove (s + offset) (e + offset)

makeAlt :: NFA Int -> NFA Int -> NFA Int
makeAlt (NFA ls lm _ _) (NFA rs rm _ _) =
    NFA (newLs `S.union` newRs `S.union` newStates)
        (newLm `S.union` newRm `S.union` newMoves)
        0
        (S.singleton finalState)
    where
        lSize = S.size ls
        rSize = S.size rs
        finalState = lSize + rSize + 1
        newStates = S.fromList [0, finalState]
        newLs = S.map (+1) ls
        newRs = S.map (+ (lSize + 1)) rs
        newLm = renumberMoves lm 1
        newRm = renumberMoves rm (lSize + 1)
        newMoves = S.fromList [
                EMove 0 1,
                EMove 0 (lSize + 1),
                EMove lSize finalState,
                EMove (finalState - 1) finalState
            ]

makeConcat :: NFA Int -> NFA Int -> NFA Int
makeConcat (NFA ls lm _ _) (NFA rs rm _ _) =
    NFA (newLs `S.union` newRs `S.union` newStates)
        (newLm `S.union` newRm `S.union` newMoves)
        0
        (S.singleton finalState)
    where
        lSize = S.size ls
        rSize = S.size rs
        finalState = lSize + rSize - 1
        newStates = S.empty
        newLs = ls
        newRs = S.map (+ lSize) rs
        newLm = lm
        newRm = renumberMoves rm lSize
        newMoves = S.singleton (EMove (lSize - 1) lSize)

makeRep :: NFA Int -> RExpr -> NFA Int
makeRep (NFA ls lm _ _) rep = 
    NFA (newLs `S.union` newStates)
        (newLm `S.union` newMoves)
        0
        (S.singleton finalState)
    where 
        (finalState, newStates, newLs, newLm, newMoves) = getValuesForRep rep ls lm

getValuesForRep :: RExpr -> S.Set Int -> S.Set (Move Int) -> (Int, S.Set Int, S.Set Int, S.Set (Move Int), S.Set (Move Int))
getValuesForRep (Star _) ls lm = (
        finalState,
        S.fromAscList [0, finalState],
        S.map (+1) ls,
        renumberMoves lm 1,
        S.fromList [
            EMove 0 1,
            EMove 0 finalState,
            EMove lSize finalState,
            EMove lSize 1
        ]
    )
    where
        lSize = S.size ls 
        finalState = lSize + 1

getValuesForRep (Plus _) ls lm = (
        finalState,
        S.singleton finalState,
        ls,
        lm,
        S.fromList [
            EMove (finalState - 1) finalState,
            EMove (finalState - 1) 0
        ]
    )
    where
        lSize = S.size ls 
        finalState = lSize

getValuesForRep (Option _) ls lm = (
        finalState,
        S.fromAscList [0, finalState],
        S.map (+1) ls,
        renumberMoves lm 1,
        S.fromList [
            EMove 0 1,
            EMove 0 finalState,
            EMove lSize finalState
        ]
    )
    where
        lSize = S.size ls 
        finalState = lSize + 1

getValuesForRep _ _ _ = undefined
