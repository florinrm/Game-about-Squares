{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances #-}

module GAS where

import ProblemState

import qualified Data.Map.Strict as M
import Data.Char
import qualified Data.List as L
import qualified Data.Set as Set

{-
    Pozițiile tablei de joc, în formă (linie, coloană), unde ambele coordonate
    pot fi negative.
-}
type Position = (Int, Int)

{-
    Culorile pătratelor și cercurilor.
-}
data Color = Red | Blue | Gray
    deriving (Eq, Ord)

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

instance Show Color where
    show Red = "R"
    show Blue = "B"
    show Gray = "G"

{-
    *** TODO ***

    Un obiect de pe tabla de joc: pătrat/ cerc/ săgeată.
-}
data Object = CreateSquare {orientation :: Heading, color :: Color, shape :: Shape}
            | CreateCircle {color :: Color, shape :: Shape}
            | CreateArrow {direction :: Heading, shape :: Shape}
            | EmptyPos
    deriving (Eq, Ord)

{-
    *** TODO ***

    Reprezetarea textuală a unui obiect.
-}
instance Show Object where
    show EmptyPos = "   "
    show (CreateSquare orient col _) = (show col) ++ (show orient) ++ " "
    show (CreateCircle col _) = "  " ++ [(toLower (head (show col)))]
    show (CreateArrow dir _) = "  " ++ (show dir)

{-
    *** TODO ***

    Un nivel al jocului.

    Recomandăm Data.Map.Strict.
-}

data Level = CreateLevel {elements :: (M.Map Position [Object])}
    deriving (Eq, Ord)

{-
    *** TODO ***

    Reprezetarea textuală a unui nivel.
-}

getHeightLevel :: Level -> Int 
getHeightLevel lvl = 1 + maximum (map fst (map fst (M.toList (elements lvl)))) - minimum (map fst (map fst (M.toList (elements lvl)))) 

getLengthLevel :: Level -> Int 
getLengthLevel lvl = 1 + maximum (map snd (map fst (M.toList (elements lvl)))) - minimum (map snd (map fst (M.toList (elements lvl)))) 


getMaxCol :: Level -> Int
getMaxCol lvl = maximum (map snd (map fst (M.toList (elements lvl))))
getMinCol :: Level -> Int
getMinCol lvl = minimum (map snd (map fst (M.toList (elements lvl))))
getMaxLine :: Level -> Int
getMaxLine lvl = maximum (map fst (map fst (M.toList (elements lvl))))
getMinLine :: Level -> Int
getMinLine lvl = minimum (map fst (map fst (M.toList (elements lvl)))) 

fillInColumn :: Level -> Int -> Int -> Int -> Level
fillInColumn lvl begin end line
        | begin == (end + 1) = lvl
        | ((elements lvl) M.!? pair) /= Nothing = fillInColumn lvl (begin + 1) end line
        | otherwise = CreateLevel (M.insert pair [EmptyPos] (elements (fillInColumn lvl (begin + 1) end line)))
            where pair :: Position; pair = (line, begin)

fillInLine :: Level -> Int -> Int -> Level
fillInLine lvl begin end
        | begin == (end + 1) = lvl
        | otherwise = fillInLine (fillInColumn lvl (getMinCol lvl) (getMaxCol lvl) begin) (begin + 1) end

fillIn :: Level -> Level
fillIn lvl = if lvl == emptyLevel 
                then lvl
            else fillInLine lvl (getMinLine lvl) (getMaxLine lvl)

takeSquare :: [Object] -> Object
takeSquare lst = head $ filter (\x -> Square == (shape x)) lst

takeCircle :: [Object] -> Object
takeCircle lst = head $ filter (\x -> Circle == (shape x)) lst

takeArrow :: [Object] -> Object
takeArrow lst = head $ filter (\x -> Arrow == (shape x)) lst

printTwoShapes :: [Object] -> [Char]
printTwoShapes lst
    | (shape one == Square) && (shape two == Circle) = (head (show one)) : (head (tail (show one))) : (last (show two)) : []
    | (shape two == Square) && (shape one == Circle) = (head (show two)) : (head (tail (show two))) : (last (show one)) : []
    | (shape one == Square) && (shape two == Arrow) = (head (show one)) : (head (tail (show one))) : (last (show two)) : []
    | (shape two == Square) && (shape one == Arrow) = (head (show two)) : (head (tail (show two))) : (last (show one)) : []
    | otherwise = [] 
        where
            one = head lst
            two = last lst

printCol :: Level -> Int -> Int -> Int -> Int -> [Char]
printCol lvl begin end curr line
            | curr == (end + 1) = []
            | curr == end = if (length ((elements lvl) M.! pos)) /= 2
                                    then (show (head ((elements lvl) M.! pos))) ++ (printCol lvl begin end (curr + 1) line)
                            else
                                (printTwoShapes ((elements lvl) M.! pos)) ++ (printCol lvl begin end (curr + 1) line)
            | otherwise = if (length ((elements lvl) M.! pos)) /= 2
                                    then (show (head ((elements lvl) M.! pos))) ++ "|" ++ (printCol lvl begin end (curr + 1) line)
                            else
                                (printTwoShapes ((elements lvl) M.! pos)) ++ "|" ++ (printCol lvl begin end (curr + 1) line)
                                where
                                    pos :: Position; pos = (line, curr)
printLine :: Level -> Int -> Int -> Int -> [Char]
printLine lvl begin end curr
            | curr == (end + 1) = []
            | curr == end = (printCol lvl (getMinCol lvl) (getMaxCol lvl) (getMinCol lvl) curr) ++ (printLine lvl begin end (curr + 1))
            | otherwise = (printCol lvl (getMinCol lvl) (getMaxCol lvl) (getMinCol lvl) curr) ++ "\n" ++ (printLine lvl begin end (curr + 1))

printTable :: Level -> [Char]
printTable lvl = printLine (fillIn lvl) (getMinLine (fillIn lvl)) (getMaxLine (fillIn lvl)) (getMinLine (fillIn lvl))

instance Show Level where
    show (CreateLevel mapping) = printTable (CreateLevel mapping)

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
addSquare col heading pos lvl 
    | ((elements lvl) M.!? pos) == Nothing =  CreateLevel (M.insert pos [(CreateSquare heading col Square)] (elements lvl))
    | otherwise = CreateLevel (M.insert pos ((CreateSquare heading col Square) : ((elements lvl) M.! pos)) (elements lvl))

{-
    *** TODO ***

    Adaugă un cerc cu caracteristicile date la poziția precizată din nivel.
-}
addCircle :: Color -> Position -> Level -> Level
addCircle col pos lvl    
    | ((elements lvl) M.!? pos) == Nothing =  CreateLevel (M.insert pos [(CreateCircle col Circle)] (elements lvl))
    | otherwise = CreateLevel (M.insert pos ((CreateCircle col Circle) : ((elements lvl) M.! pos)) (elements lvl))

{-
    *** TODO ***

    Adaugă o săgeată cu caracteristicile date la poziția precizată din nivel.
-}
addArrow :: Heading -> Position -> Level -> Level
addArrow heading pos lvl    
    | ((elements lvl) M.!? pos) == Nothing =  CreateLevel (M.insert pos [(CreateArrow heading Arrow)] (elements lvl))
    | otherwise = CreateLevel (M.insert pos ((CreateArrow heading Arrow) : ((elements lvl) M.! pos)) (elements lvl))

{-
    *** TODO ***

    Mută pătratul de la poziția precizată din nivel. Dacă la poziția respectivă
    nu se găsește un pătrat, întoarce direct parametrul.
-}

movePosition :: Position -> Object -> Position
movePosition pos obj
        | (orientation obj == East) = ((fst pos), (snd pos) + 1)
        | (orientation obj == West) = ((fst pos), (snd pos) - 1)
        | (orientation obj == North) = ((fst pos) - 1, (snd pos))
        | otherwise = ((fst pos) + 1, (snd pos))  -- modific pozitia unui patrat

data SquareCircle = SC {square :: Object, circle :: Object}

modifySquare :: Object -> Position -> Level -> Object
modifySquare sqr pos level
            | res == [] = sqr
            | otherwise = CreateSquare (direction $ head $ res) (color sqr) Square
                where res = filter (\x -> (shape x) == Arrow) ((elements level) M.! pos) -- modific directia unui patrat
    

getSquares :: Level -> Position -> [Object]
getSquares lvl pos = (filter (\x -> Square == (shape x)) ((elements lvl) M.! pos))

getArrows :: Level -> Position -> [Object]
getArrows lvl pos = (filter (\x -> Arrow == (shape x)) ((elements lvl) M.! pos))

removeEmptyValues :: Level -> Position -> Level
removeEmptyValues lvl pos
    | ((elements lvl) M.!? pos) == Nothing = lvl
    | ((elements lvl) M.! pos) == [] = CreateLevel (M.delete pos (elements lvl))
    | otherwise = lvl

-- muta patratul de la pozitia noua -> recursiv, ca sa dai push si la celelalte patrate

data SomeData = CreateData {oldPos :: Position, newPos :: Position, obj :: Object} deriving (Eq, Ord)
instance Show SomeData where
    show (CreateData a b c) = (show a) ++ " " ++ (show b) ++ " " ++ (show c)

findSouthSquares :: Level -> Position -> [SomeData]
findSouthSquares lvl pos
        | ((elements lvl) M.!? pos) == Nothing = []
        | [] == (getSquares lvl pos) = []
        | otherwise = (CreateData pos newpos sqr) : (findSouthSquares lvl newpos)
                    where sqr = (head (getSquares lvl pos))
                          newpos :: Position; newpos = ((fst pos) + 1, (snd pos))

findNorthSquares :: Level -> Position -> [SomeData]
findNorthSquares lvl pos
        | ((elements lvl) M.!? pos) == Nothing = []
        | [] == (getSquares lvl pos) = []
        | otherwise = (CreateData pos newpos sqr) : (findNorthSquares lvl newpos)
                    where sqr = (head (getSquares lvl pos))
                          newpos :: Position; newpos = ((fst pos) - 1, (snd pos))

findEastSquares :: Level -> Position -> [SomeData]
findEastSquares lvl pos
        | ((elements lvl) M.!? pos) == Nothing = []
        | [] == (getSquares lvl pos) = []
        | otherwise = (CreateData pos newpos sqr) : (findEastSquares lvl newpos)
                    where sqr = (head (getSquares lvl pos))
                          newpos :: Position; newpos = ((fst pos), (snd pos) + 1)

findWestSquares :: Level -> Position -> [SomeData]
findWestSquares lvl pos
        | ((elements lvl) M.!? pos) == Nothing = []
        | [] == (getSquares lvl pos) = []
        | otherwise = (CreateData pos newpos sqr) : (findWestSquares lvl newpos)
                    where sqr = (head (getSquares lvl pos))
                          newpos :: Position; newpos = ((fst pos), (snd pos) - 1)

getSquareList :: Level -> Position -> Object -> [SomeData]
getSquareList lvl pos obj
        | Square /= (shape obj) = []
        | North == (orientation obj) = findNorthSquares lvl pos
        | South == (orientation obj) = findSouthSquares lvl pos
        | West == (orientation obj) = findWestSquares lvl pos
        | otherwise = findEastSquares lvl pos

changePos :: Level -> Position -> Position -> Object -> Level
changePos lvl oldpos newpos obj
        | Square /= (shape obj) = lvl
        | ((elements lvl) M.!? oldpos) == Nothing = lvl
        | (notElem obj ((elements lvl) M.! oldpos)) = lvl
        | ((elements lvl) M.!? newpos) == Nothing =
            (removeEmptyValues (CreateLevel (M.insert newpos [obj] (M.insert oldpos (L.delete obj ((elements lvl) M.! oldpos)) (elements lvl)))) oldpos)
        | otherwise = 
            (removeEmptyValues (CreateLevel (M.insert newpos (newobj : ((elements lvl) M.! newpos)) (M.insert oldpos (L.delete obj ((elements lvl) M.! oldpos)) (elements lvl)))) oldpos)
                where newobj = modifySquare obj newpos lvl

changeAllPos :: Level -> [SomeData] -> Level
changeAllPos lvl lst
        | lst == [] = lvl
        | otherwise = changePos (changeAllPos lvl (tail lst)) (oldPos res) (newPos res) (obj res)
            where res = head lst
{--
    vad cum e directia patratului - sus, jos, stanga dreapta
    daca e in jos - caut patratele de pe aceeasi coloana si le dau in jos pe toate+ automat schimb directia -> caut primul patrat de josul lui si o fac recursiv
    analog 
--}
move :: Position  -- Poziția
     -> Level     -- Nivelul inițial
     -> Level     -- Nivelul final

move pos lvl 
    | ((elements lvl) M.!? pos) == Nothing = lvl
    | [] == (getSquares lvl pos) = lvl  -- daca nu am patrat la pozitia data
    | ((elements lvl) M.!? (movePosition pos (head (getSquares lvl pos)))) == Nothing =     -- > daca nu am exista pozitia noua
        (removeEmptyValues (CreateLevel (M.insert (movePosition pos (head (getSquares lvl pos))) [(head (getSquares lvl pos))] 
            (M.insert pos (L.delete (head (getSquares lvl pos)) ((elements lvl) M.! pos)) (elements lvl)))) pos)
    | [] == (getSquares lvl (movePosition pos (head (getSquares lvl pos)))) =  -- daca exista nu exista patrat la pozitia noua
            if (getArrows lvl (movePosition pos (head (getSquares lvl pos)))) /= [] -- daca nu am sageti la pozitia noua
                then (removeEmptyValues (CreateLevel (M.insert (movePosition pos (head (getSquares lvl pos))) 
                        ((modifySquare (head (getSquares lvl pos)) (movePosition pos (head (getSquares lvl pos))) lvl)
                         : ((elements lvl) M.! (movePosition pos (head (getSquares lvl pos))))) 
                        (M.insert pos (L.delete (head (getSquares lvl pos)) ((elements lvl) M.! pos)) (elements lvl)))) pos)--aww yiss
                else (removeEmptyValues (CreateLevel (M.insert (movePosition pos (head (getSquares lvl pos))) 
                        ((head (getSquares lvl pos)) : ((elements lvl) M.! (movePosition pos (head (getSquares lvl pos))))) 
                        (M.insert pos (L.delete (head (getSquares lvl pos)) ((elements lvl) M.! pos)) (elements lvl)))) pos)-- aww yiss
    | otherwise = changeAllPos lvl (getSquareList lvl pos (head (getSquares lvl pos)))
 -- (move (movePosition pos (head (getSquares lvl pos))) lvl)    
  
---}
{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru.
-}

checkState :: [Object] -> Bool
checkState lst
        | 1 >= (length lst) = False
        | (shape (head lst)) == Circle && (shape (last lst) == Square) = (color (head lst)) == (color (last lst))
        | (shape (last lst)) == Circle && (shape (head lst) == Square) = (color (head lst)) == (color (last lst))
        | otherwise = False

numberOfSquares :: Level -> Int
numberOfSquares lvl = length (filter (\x -> [] /= (getSquares lvl (fst x))) (M.toList (elements lvl)))

numberValid :: Level -> Int
numberValid lvl = length (filter (\x -> True == (checkState (snd x))) (M.toList (elements lvl)))

getSquarePositions :: Level -> [Position]
getSquarePositions lvl = map fst (filter (\x -> [] /= (getSquares lvl (fst x))) (M.toList (elements lvl)))

buildSuccesors :: Level -> [Position] -> [(Position, Level)]
buildSuccesors lvl pos = if [] == pos
                            then []
                         else
                            ((head pos), (move (head pos) lvl)) : (buildSuccesors lvl (tail pos))   

instance ProblemState Level Position where
    successors lvl = buildSuccesors lvl (getSquarePositions lvl)

    isGoal lvl = (numberOfSquares lvl) == (numberValid lvl)

    -- Doar petru BONUS
    -- heuristic =
{--
        iau patratul, vad ce directie are, iau o pereche ((pos_vechi, pos_nou), object) si fac insertie de pe poz veche pe poz noua (+ faza cu directia lel)
--}