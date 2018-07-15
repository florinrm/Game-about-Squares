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
data Node s a = CreateNode s a s Int [s] -- nod, actiune, nodul parinte, adancime, lista cu copii
    deriving (Eq, Show, Ord)

{-
    *** TODO ***

    Întoarce starea stocată într-un nod.
-}
nodeState :: Node s a -> s
nodeState (CreateNode s _ _ _ _) = s

nodeAction :: Node s a -> a
nodeAction (CreateNode _ a _ _ _) = a

parentNode :: Node s a -> s
parentNode (CreateNode _ _ s _ _) = s

nodeDepth :: Node s a -> Int
nodeDepth (CreateNode _ _ _ len _) = len

nodeList :: Node s a -> [s]
nodeList (CreateNode _ _ _ _ list) = list

{-
    *** TODO ***

    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la starea dată ca parametru.

    Pentru reținerea stărilor vizitate, recomandăm Data.Set. Constrângerea
    `Ord s` permite utilizarea tipului `Set`.

    În afara BONUS-ului, puteți ignora parametrul boolean. Pentru BONUS, puteți
    sorta lista succesorilor folosind `sortBy` din Data.List.
-}


limitedDfsHelper :: (ProblemState s a, Ord s, Ord a) => [Node s a] -> [Node s a] -> S.Set s -> Int -> Bool -> [Node s a]
limitedDfsHelper lvls [] _ _ _ = reverse lvls
limitedDfsHelper lvls (elem : list) set maxDepth check
    | ((S.member node set) || (depth > maxDepth)) = limitedDfsHelper lvls list set maxDepth check
    | ((isGoal node) && (check == True)) = limitedDfsHelper ((head lvls) : elem : (tail lvls)) (elements ++ list) (S.insert node set) maxDepth check
    | otherwise = limitedDfsHelper (elem : lvls) (elements ++ list) (S.insert node set) maxDepth check
        where
            node = nodeState elem
            depth = nodeDepth elem
            elements = [CreateNode state action node (depth + 1) (state : (nodeList elem)) | action <- (map fst (successors node)), state <- (map snd (successors node))] 


limitedDfs :: (ProblemState s a, Ord s, Ord a)
           => s           -- Starea inițială
           -> Bool        -- Pentru BONUS, `True` dacă utilizăm euristica
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri
limitedDfs initialState check depth = limitedDfsHelper [] [(CreateNode initialState (head (map fst (successors initialState))) initialState 0 [initialState])] S.empty depth check

{-
    *** TODO ***

    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.

    În afara BONUS-ului, puteți ignora parametrul boolean.
-}

getIndex :: Eq s => s -> [s] -> Int
getIndex state listStates = foldl (\acc elem -> if (fst elem) == state then snd elem else acc) 0 (zip listStates [0..(length listStates) - 1])
                                            
getNode :: Eq s => s -> [Node s a] -> Node s a
getNode state listStates = head (filter (\elem -> (nodeState elem) == state) listStates)

iterativeDeepening :: (ProblemState s a, Ord s, Ord a)
    => s                -- Starea inițială
    -> Bool             -- Pentru BONUS, `True` dacă utilizăm euristica
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
iterativeDeepening s b = foldl (\acc depth -> let goal = filter (isGoal) (map (nodeState) (limitedDfs s b depth)) in          
                                                if (not ((nodeState (fst acc)) == s)) 
                                                    then acc 
                                                else if (not ((length goal) == 0))                 
                                                    then ((CreateNode (head goal) randomValue s depth (nodeList (getNode (head goal) (limitedDfs s b depth)))), 
                                                        ((snd acc) + (getIndex (head goal) (map (nodeState) (limitedDfs s b depth)))))
                                                else 
                                                    ((CreateNode s randomValue s 0 []), ((snd acc) + (length (limitedDfs s b depth))))) ((CreateNode s randomValue s 0 []), 0) [0..999]
                                                    
    where
        randomValue = head (map (fst) (successors s))
            

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

filterAction :: (Ord s) => [(a, s)] -> s -> a
filterAction list state = fst (head (filter (\lel -> state == (snd lel)) list))



extractPath :: (ProblemState s a, Ord s, Ord a) => Node s a -> [(a, s)]
extractPath (CreateNode _ _ _ _ list) = reverse (snd (foldl (\acc lel -> (lel, ((filterAction (successors (fst acc)) lel), lel) : (snd acc))) (headList, []) restList))
            where
                revList = reverse list
                headList = head revList
                restList = tail revList

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}
printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))