# Battle for Aldebaran

A retro-style game implemented in Haskell with FunGEn

![GitHub Logo](http://screenlist.ru/data/media/6/battle.png)

# Download and Build

You can download a Windows binary distribution from the [releases page](https://github.com/EvgeniyaKatunina/BattleForAldebaran/releases).

The game is built with Cabal, use `cabal build` or `cabal run` after installing the required packages.

You can play around with the game parameters, changing them in [`GameParameters.hs`](https://github.com/EvgeniyaKatunina/BattleForAldebaran/blob/master/src/GameParameters.hs).

# Gameplay

The gameplay is quite straightforward: you control a starfleet of several ships (one at a time) and your goal is to destroy all enemy ships. 
What complicates the matter is that the ships and the gunshots and everyhing moves in the orbits.

# Acknowledgements

This	work	would	not	have	been	possible without the [FunGEn game engine](http://joyful.com/fungen/) and my cat Barsik.