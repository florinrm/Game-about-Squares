{-# OPTIONS_GHC -Wall #-}

module Search where

import ProblemState

import qualified Data.Set as S

{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime.
-}
data Node s a = CreateNode {state :: s, pos :: a}
    deriving (Eq, Show, Ord)

{-
    *** TODO ***

    Întoarce starea stocată într-un nod.
-}
nodeState :: Node s a -> s
nodeState node = state node

{-
    *** TODO ***

    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la starea dată ca parametru.

    Pentru reținerea stărilor vizitate, recomandăm Data.Set. Constrângerea
    `Ord s` permite utilizarea tipului `Set`.

    În afara BONUS-ului, puteți ignora parametrul boolean. Pentru BONUS, puteți
    sorta lista succesorilor folosind `sortBy` din Data.List.
-}

removeDuplicatesTail :: (ProblemState s a, Ord s, Ord a) => [Node s a] -> [Node s a] -> [Node s a]
removeDuplicatesTail lst acc
    | [] == lst = acc
    | (elem (state (head lst)) (map state acc)) = removeDuplicatesTail (tail lst) acc
    | otherwise = removeDuplicatesTail (tail lst) (acc ++ [head lst])

dfsSuccesor :: (ProblemState s a, Ord s, Ord a) => [(a, s)] -> (S.Set (Node s a)) -> Int -> [Node s a]
dfsSuccesor succ set depth
    | succ == [] = []
    | depth == 0 = []
    | (S.member node set) = []
    | otherwise = node : (limitedDfsHelper (state node) (depth - 1) (S.insert node set)) 
                    ++ (dfsSuccesor (tail succ) (S.insert node set) depth)
        where 
            element = head succ
            node = CreateNode (snd element) (fst element)

limitedDfsHelper :: (ProblemState s a, Ord s, Ord a) => s -> Int -> (S.Set (Node s a)) -> [Node s a]
limitedDfsHelper lvl depth visited
    | depth == 0 = []
    | (isGoal lvl) == True = []
    | otherwise = removeDuplicatesTail (dfsSuccesor (successors lvl) visited depth) []


limitedDfs :: (ProblemState s a, Ord s, Ord a)
           => s           -- Starea inițială
           -> Bool        -- Pentru BONUS, `True` dacă utilizăm euristica
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri
limitedDfs initialState check depth 
    | depth == 0 = [(CreateNode initialState undefined)]
    | otherwise = (CreateNode initialState undefined) : limitedDfsHelper initialState depth S.empty

{-
    *** TODO ***

    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.

    În afara BONUS-ului, puteți ignora parametrul boolean.
-}
iterativeDeepening :: (ProblemState s a, Ord s)
    => s                -- Starea inițială
    -> Bool             -- Pentru BONUS, `True` dacă utilizăm euristica
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
iterativeDeepening state check = undefined

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}
extractPath :: Node s a -> [(a, s)]
extractPath = undefined

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}
printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))