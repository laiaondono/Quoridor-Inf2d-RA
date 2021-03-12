{-
    Module: Reed.

    *** PART III (10 pt) ***

    Define a player that uses teh Reed opening and play against it. Is the Reed opening a good 
    opening? Write your answers in Reed.txt.
-}
module Players.Reed where

import Types
import Action
import Game
import Players.Dumb (dumbAction)

-- Create a player that starts with the Reed opening. After that, you may use your minimax action or
-- the given action for DumbPlayer. 
-- [Hint 1: Use the variable 'turn' in Player.]
-- [Hint 2: Use 'wallTop' to get the walls you need.]
-- [Hint 3: Don't forget to check that the action is valid using 'validWallAction'.]
reedPlayerAction :: Board -> [Player] -> String -> Int -> Maybe Action
reedPlayerAction b ps s r = 
    let current_player = currentPlayer ps -- asumption: reed player is player 2
        current_turn = getTurnPlayer current_player
    in chooseActionReed b ps s r current_turn

getTurnPlayer :: Player -> Int
getTurnPlayer (Player _ turn _ _ _ _ _) = turn

chooseActionReed :: Board -> [Player] -> String -> Int -> Int -> Maybe Action
chooseActionReed b ps s r turn
    | turn <= 1 && validWallAction (Game b ps) (wallTop ('c', 3)) = Just (Place (wallTop ('c', 3))) -- first wall of the reed opening (if possible)
    | turn <= 2 && validWallAction (Game b ps) (wallTop ('f', 3)) = Just (Place (wallTop ('f', 3))) -- second wall of the reed opening (if possible)
    | otherwise = dumbAction b ps s r -- after the opening, reed player is dumb player

-- We build a Reed player from a name, a starting cell, a number of walls, an array of winning
-- positions and 'commandToAction'.
makeReedPlayer :: String -> Cell -> Int -> [Cell] -> Player
makeReedPlayer n c rws wps = Player {
    name = n,
    turn = 1,
    currentCell = c, 
    remainingWalls = rws,
    winningPositions = wps,
    isHuman = False,
    chooseAction = reedPlayerAction } 
