EXTENSION: ALLOW 4-PLAYER GAMES
Files modified: Main.hs

In the beginning of the game, you will be able to choose whether you want to have a 2-player or 4-player game.


EXTENSION: ALLOW DIAGONAL MOVES
Files modified: Action.hs, Cell.hs

Basic tests: OK (all pass).
Minimax tests:
    - Part I.a to Part I.d: OK (all pass).
    - Part I.e unit test testUtilityPositionAhead: it will take 2-3 minuts to finish it with boardSize = 5, 
      if you change it to boardSize = 3 it will be faster.
    - Part I.e unit test testUtilityWallInFront: this test fails because as diagonal moves are allowed, 
      it can happen that placing a wall is no longer effective to decrease the utility value of a cell.
      Example:
        If we have:

        a5-- Y -- c5-- d5-- e5 
        |    |    |    |    |  
        |    |    |    |    |  
        a4-- b4-- c4-- d4-- e4 
        |    |    |    |    |  
        |    |    |    |    |  
        a3-- b3-- c3-- d3-- e3 
        |    |              |  
        |    |              |  
        a2-- b2-- c2-- X -- e2 
        |    |    |    |    |  
        |    |    |    |    |  
        a1-- b1-- c1-- d1-- e1 

        Without diagonal moves X had to go to first to e2 and later to e3. However, with diagonal moves, 
        X can go straight to e3, which shows how the placements of this wall is useless, given that it 
        will not affect the utility function, causing the test to fail.
