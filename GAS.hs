{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances #-}

module GAS where

import ProblemState

import qualified Data.Map.Strict as M

{-
    Pozițiile tablei de joc, în formă (linie, coloană), unde ambele coordonate
    pot fi negative.
-}
type Position = (Int, Int)

{-
    Culorile pătratelor și cercurilor.
-}
data Color = Red | Blue | Gray
    deriving (Eq, Ord, Show)

data Shape = Square | Circle | Arrow | Empty
    deriving (Eq, Ord, Show)

{-
    Orientările pătratelor și săgeților.
-}
data Heading = North | South | East | West
    deriving (Eq, Ord)

instance Show Heading where
    show North = "^"
    show South = "v"
    show East  = ">"
    show West  = "<"

{-
    *** TODO ***

    Un obiect de pe tabla de joc: pătrat/ cerc/ săgeată.
-}
data Object = CreateSquare {orientation :: Char, color :: Char, shape :: Shape}
            | CreateCircle {color :: Char, shape :: Shape}
            | CreateArrow {direction :: Char, shape :: Shape}
            | EmptyPos
    deriving (Eq, Ord)

{-
    *** TODO ***

    Reprezetarea textuală a unui obiect.
-}
instance Show Object where
    show EmptyPos = "   "
    show (CreateSquare orient col _) = [col] ++ [orient] ++ " "
    show (CreateCircle col _) = "  " ++ [col]
    show (CreateArrow dir _) = "  " ++ [dir]

{-
    *** TODO ***

    Un nivel al jocului.

    Recomandăm Data.Map.Strict.
-}

data Level = CreateLevel {elements :: (M.Map Object Position)}
    deriving (Eq, Ord)

{-
    *** TODO ***

    Reprezetarea textuală a unui nivel.
-}

getLengthLevel :: Level -> Int 
getLengthLevel lvl = 1 + maximum (map fst (map snd (M.toList (elements lvl)))) - minimum (map fst (map snd (M.toList (elements lvl)))) 

getHeightLevel :: Level -> Int 
getHeightLevel lvl = 1 + maximum (map snd (map snd (M.toList (elements lvl)))) - minimum (map snd (map snd (M.toList (elements lvl)))) 

instance Show Level where
    show (CreateLevel mapping) = " "

{-
    *** TODO ***

    Nivelul vid, fără obiecte.
-}
emptyLevel :: Level
emptyLevel = CreateLevel (M.fromList [])

{-
    *** TODO ***

    Adaugă un pătrat cu caracteristicile date la poziția precizată din nivel.
-}
addSquare :: Color -> Heading -> Position -> Level -> Level
addSquare color heading pos lvl 
            | color == Red && heading == North = CreateLevel (M.insert (CreateSquare 'R' '^' Square) pos (elements lvl))
            | color == Red && heading == East = CreateLevel (M.insert (CreateSquare 'R' '>' Square) pos (elements lvl))
            | color == Red && heading == South = CreateLevel (M.insert (CreateSquare 'R' 'v' Square) pos (elements lvl))
            | color == Red && heading == West = CreateLevel (M.insert (CreateSquare 'R' '<' Square) pos (elements lvl))
            | color == Blue && heading == North = CreateLevel (M.insert (CreateSquare 'B' '^' Square) pos (elements lvl))
            | color == Blue && heading == East = CreateLevel (M.insert (CreateSquare 'B' '>' Square) pos (elements lvl))
            | color == Blue && heading == South = CreateLevel (M.insert (CreateSquare 'B' 'v' Square) pos (elements lvl))
            | color == Blue && heading == West = CreateLevel (M.insert (CreateSquare 'B' '<' Square) pos (elements lvl))
            | color == Gray && heading == North = CreateLevel (M.insert (CreateSquare 'G' '^' Square) pos (elements lvl))
            | color == Gray && heading == East = CreateLevel (M.insert (CreateSquare 'G' '>' Square) pos (elements lvl))
            | color == Gray && heading == South = CreateLevel (M.insert (CreateSquare 'G' 'v' Square) pos (elements lvl))
            | color == Gray && heading == West = CreateLevel (M.insert (CreateSquare 'G' '<' Square) pos (elements lvl))
            | otherwise = lvl

{-
    *** TODO ***

    Adaugă un cerc cu caracteristicile date la poziția precizată din nivel.
-}
addCircle :: Color -> Position -> Level -> Level
addCircle color pos lvl
            | color == Red = CreateLevel (M.insert (CreateCircle 'r' Circle) pos (elements lvl))
            | color == Blue = CreateLevel (M.insert (CreateCircle 'b' Circle) pos (elements lvl))
            | color == Gray = CreateLevel (M.insert (CreateCircle 'g' Circle) pos (elements lvl))
            | otherwise = lvl

{-
    *** TODO ***

    Adaugă o săgeată cu caracteristicile date la poziția precizată din nivel.
-}
addArrow :: Heading -> Position -> Level -> Level
addArrow heading pos lvl
            | heading == North = CreateLevel (M.insert (CreateArrow '^' Arrow) pos (elements lvl))
            | heading == East = CreateLevel (M.insert (CreateArrow '>' Arrow) pos (elements lvl))
            | heading == South = CreateLevel (M.insert (CreateArrow 'v' Arrow) pos (elements lvl))
            | heading == West = CreateLevel (M.insert (CreateArrow '<' Arrow) pos (elements lvl))
            | otherwise = lvl

{-
    *** TODO ***

    Mută pătratul de la poziția precizată din nivel. Dacă la poziția respectivă
    nu se găsește un pătrat, întoarce direct parametrul.
-}

movePosition :: Position -> Object -> Position
movePosition pos obj
        | (orientation obj == '>') = ((fst pos) + 1, (snd pos))
        | (orientation obj == '<') = ((fst pos) - 1, (snd pos))
        | (orientation obj == '^') = ((fst pos), (snd pos) + 1)
        | otherwise = ((fst pos), (snd pos) - 1)

move :: Position  -- Poziția
     -> Level     -- Nivelul inițial
     -> Level     -- Nivelul final
move pos lvl =  --filter (\x -> shape (fst x) == Square && (snd x) == pos) (toList (elements lvl))
    if res == [] then
        lvl
    else
        CreateLevel (M.insert (fst (head res)) (movePosition pos (fst (head res))) (elements lvl)) 
            where res = filter (\x -> shape (fst x) == Square && (snd x) == pos) (M.toList (elements lvl))


{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru.
-}
instance ProblemState Level Position where
    successors = undefined

    isGoal = undefined

    -- Doar petru BONUS
    -- heuristic =
