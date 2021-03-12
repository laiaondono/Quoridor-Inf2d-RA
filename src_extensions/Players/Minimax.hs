{-
    Module: Minimax.

    *** PART I (60pt) and PART II (10pt) *** 
-}
module Players.Minimax where 

import Data.Maybe
import Data.Graph
import Data.Ord
import Data.Tree
import Data.List
import Data.Array

import Types
import Constants
import Cell
import Action
import Board 
import Player
import Game
import Players.Dumb (dumbAction)

{-
    StateTree util.
-}

-- Map a function through the nodes of the tree.
mapStateTree :: (v -> w) -> StateTree v a -> StateTree w a
mapStateTree f (StateTree x ts) = StateTree (f x) [(a, mapStateTree f t) | (a, t)<-ts]

-- Calculate the depth of the tree (used to test pruneDepth).
stateTreeDepth :: StateTree v a -> Int 
stateTreeDepth (StateTree _ []) = 0
stateTreeDepth (StateTree _ ts) = 1 + (maximum (map (stateTreeDepth . snd) ts))

-- Calculate the breadth of the tree (used to test pruneBreadth).
stateTreeBreadth :: StateTree v a -> Int
stateTreeBreadth (StateTree _ []) = 0
stateTreeBreadth (StateTree _ ts) = max (length ts) (maximum (map (stateTreeBreadth . snd) ts))

{-
    Result util.
-}

-- Negating the result is simply negating the score. You may ignore this although it may be useful
-- to implement the minimax algorithm.
negResult :: Result -> Result
negResult (Result x as) = Result (-x) as

{- 
    *** Part I.a (10pt) ***

    First, we will generate a tree containing all the possible game states.
-}

-- Given a game, return a tree that encodes all the possible future game states.
-- [Hint: Use 'validActions' and 'performAction'.]
-- [Note: To speed things up, you may want to, at this stage, heuristically select which actions are 
--  more relevant. In particular, you probably don't want to consider every single possible wall.]
generateGameTree :: Game -> GameTree
generateGameTree g = generateStateTree g (validActions g)

generateStateTree :: Game -> [Action] -> StateTree Game Action
generateStateTree g [] = StateTree g []
generateStateTree g (a:as) = 
    let maybe_g = performAction g a
        new_g = fromJust maybe_g
        new_actions = validActions new_g
    in  StateTree g [(a, generateStateTree new_g new_actions)]


{-
    *** PART I.b (5pt) ***

    Re-order the tree so that when traversed by the minimax algorithm, when it traverses the 
    branches at each node, finds either the higher scores or the lower scores first, depending on
    the depth of the tree.
-}

-- Higher scoring nodes go first.
highFirst :: (Ord v) => StateTree v a -> StateTree v a
highFirst (StateTree v a) = StateTree v (sortByStateTreeValueDesc a)

sortByStateTreeValueDesc :: (Ord v) => [(a, StateTree v a)] -> [(a, StateTree v a)]
sortByStateTreeValueDesc [] = []
sortByStateTreeValueDesc (b:bs) = 
    let bigger_elements = [e | e <- bs, getValueOfStateTree e >= getValueOfStateTree b]
        smaller_elements = [e | e <- bs, getValueOfStateTree e < getValueOfStateTree b]
        bigger_sorted = sortByStateTreeValueDesc bigger_elements
        smaller_sorted = sortByStateTreeValueDesc smaller_elements
        new_b = (fst b, highFirst (snd b))
    in bigger_sorted ++ [new_b] ++ smaller_sorted

getValueOfStateTree :: (Ord v) => (a, StateTree v a) -> v
getValueOfStateTree (_, StateTree v _) = v


{-
    *** Part I.c (5pt) ***

    We don't want to look at all possible future game states as that would consume too much time and
    memory. Instead, we will only look a given number of steps into the future. Formally, the future
    game states are encoded in a tree, so we need a function that reduces the depth of a tree.
-}

-- Given a depth and a tree, return the same tree but cutting off the branches when the depth is 
-- exceeded. 
-- [Hint: You may want to use guards and recursion.]
pruneDepth :: Int -> StateTree v a -> StateTree v a
pruneDepth d (StateTree v as)
    | d <= 0 = StateTree v []
    | otherwise = StateTree v [(fst a, pruneDepth (d - 1) (snd a)) | a <- as]


{-
    *** Part I.d (5pt) ***

    Similarly, we can also make our tree smaller by not considering all the possible game states at
    a given point. We need a function that reduces the breadth (or width) of a tree.
-}

-- Given a breadth (Int n) and a tree, return the same tree but only keeping the first n branches at
-- every node. 
-- [Hint: Use 'take'.]
pruneBreadth :: Int -> StateTree v a -> StateTree v a
pruneBreadth b (StateTree v as) = 
    let new_as = take b as
    in StateTree v [(fst a, pruneBreadth b (snd a)) | a <- new_as]


{-
    *** Part I.e (15pt) ***

    A crucial part of the minimax algorithm is defining a good utility function. It should measure
    how good a game position is for the current player. In our case, a game state should be better
    than another one if the player is closer to its winning positions.
-}

-- Assign a value to each game (from the point of view of the current player).
-- [Hint 1: You may want to calculate the distance between the player's current cell and its winning
--  positions.]
-- [Hint 2: One way would be to use 'reachableCells' repeatedly.]
utility :: Game -> Int 
utility (Game board ps) = 
    let current_player = currentPlayer ps
        cell = currentCell current_player -- get the current player's current cell
        winning_cells = winningPositions current_player -- get the current player's winning cells
        visited_cells = []
    in (-1) * distToWinningCell board cell winning_cells visited_cells -- calculate the distance to a winning cell and negate it

distToWinningCell :: Board -> Cell -> [Cell] -> [Cell] -> Int
distToWinningCell b c wc vc
    | c `elem` wc = 0 -- if the cell is a winning cell return 0
    | otherwise = 
        let reachable_cells = reachableCells b c
            new_vc = (c:vc) -- add current cell to visited cells to avoid infinite loops
        in foldr min 10000000 [1 + distToWinningCell b rcell wc new_vc | rcell <- reachable_cells, rcell  `notElem` new_vc]

-- Lifting the utility function to work on trees.
evalTree :: GameTree -> EvalTree 
evalTree = mapStateTree utility 


{-
    *** Part I.f (20pt) ***

    Finally, we ask you to implement the minimax algorithm. Given an evaluation tree, it should 
    return the a high scoring action (according to the minimax algorithm).
-}

-- Given an evaluation tree (it stores a score in the node and each branch is labelled with the 
-- action that leads to the next child) return a list of actions
-- [Hint 1: Use a helper function to keep track of the highest and lowest scores.]
-- [Hint 2: Use the 'Result' datatype.]
minimaxFromTree :: EvalTree -> Action
minimaxFromTree (StateTree i as) = 
    let values = [minimaxFromTree' [a] et | (a, et) <- as] -- get the results of the subtrees
        max_value = getMaxResult values -- choose the one with the higher value
    in getFirstActionFromResult max_value -- get the first action to get to that state

minimaxFromTree' :: [Action] -> EvalTree -> Result
minimaxFromTree' acts (StateTree i as)
    | null as = Result i acts -- if no subtrees return the value of that node
    | otherwise = 
        let values = [minimaxFromTree' (acts ++ [a]) et | (a, et) <- as] -- get the results of the subtrees
            max_value = getMaxResult values -- choose the one with the higher value
            new_value = getIntFromResult max_value
        in Result new_value acts

getIntFromResult :: Result -> Int
getIntFromResult (Result i as) = i

getFirstActionFromResult :: Result -> Action
getFirstActionFromResult (Result i as) = head as

getMaxResult :: [Result] -> Result
getMaxResult [r] = r
getMaxResult (r:rs)
    | getIntFromResult (getMaxResult rs) > getIntFromResult r = getMaxResult rs
    | otherwise = r


{-
    *** Part II (10pt) ***

    Extension of Part I.e, using alpha-beta pruning. You will need to change the 'minimax' function
    below to make it use this function.
-}

-- Same as above but now use alpha-beta pruning.
-- [Hint 1: Extend the helper function in I.e to keep track of alpha and beta.]
-- [Hint 2: Use the 'Result' datatype.]
minimaxABFromTree :: EvalTree -> Action
minimaxABFromTree (StateTree i as) = 
    let values = [minimaxFromTree' [a] et | (a, et) <- as]
        max_value = getMaxResult values
    in getFirstActionFromResult max_value

minimaxABFromTree' :: [Action] -> Int -> EvalTree -> Result
minimaxABFromTree' acts v (StateTree i as)
    | null as = Result i acts
    | otherwise = 
        -- only consider the subtrees that have a higher value than the current v (best choice found so far)
        let values = [minimaxABFromTree' (acts ++ [a]) et_i (StateTree et_i et_as) | (a, StateTree et_i et_as) <- as, v < et_i]
            max_value = getMaxResult values
            new_value = getIntFromResult max_value
        in Result new_value acts


{-
    Putting everything together.
-}

-- Given depth for pruning (should be even).
depth :: Int 
depth = 4

-- Given breadth for pruning.
breadth :: Int 
breadth = 10

-- Function that combines all the different parts implemented in Part I.
minimax :: Game -> Action
minimax =
      minimaxFromTree -- or 'minimaxABFromTree'
    . pruneBreadth breadth
    . highFirst
    . evalTree
    . pruneDepth depth
    . generateGameTree 

-- Given a game state, calls minimax and returns an action.
minimaxAction :: Board -> [Player] -> String -> Int -> Maybe Action
minimaxAction b ps _ r = let g = Game b ps in minimaxAction' g (minimax g)
    where 
        -- Goes through the list of actions until it finds a valid one. 
        minimaxAction' :: Game -> Action -> Maybe Action
        minimaxAction' g' (Move s)
            | validStepAction g' s = Just (Move s)
            | otherwise = error "Minimax chose an invalid action."
        minimaxAction' g' (Place w)
            | validWallAction g' w = Just (Place w)
            | otherwise = error "Minimax chose an invalid action."

-- Make minimaxPlayer in the usual way using 'minimaxAction'.
makeMinimaxPlayer :: String -> Cell -> Int -> [Cell] -> Player
makeMinimaxPlayer n c rws wps = Player {
    name = n,
    turn = 1,
    currentCell = c, 
    remainingWalls = rws,
    winningPositions = wps,
    isHuman = False,
    chooseAction = minimaxAction }
