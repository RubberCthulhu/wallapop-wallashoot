Solution for Wallapop wallashoot programming challenge
========================================================
I would to write a game where given a 2D space the gamer can move across the space on eight directions (up,down,right,left,up-left,up-right, down-left, down-right) and make a shoot.
* Each player can only make one movement or shoot minimum every 100ms. 
* The Game ends when only one player is alive.
* The x and y coordinates must be an integer.
* We consider the shoot velocity is instantaneous. Consider velocity will have extra points, nice to have.
* The Shoot directions is the same as player movement (up,down,right,left,up-left,up-right, down-left, down-right)
* On setup accepts x and y range when server starts
* On setup accepts the number of players
* Consider this game run on same machine. (Running in different nodes is a nice to have)
* Logs all movements and shoots of players.
* For each player store number of killed players and print a report when the game end.
* We can attach new players at any time, also after the game has start. 

Buuild
------
```
rebar get-deps
rebar compile
```

Tests
-----
```
rebar eunit skip_deps=true
rebar ct skip_deps=true
```

Build docs
----------
```
rebar doc
```
