### Functional Q-Learning in Haskell

A simple RL demonstration using Haskell
Consisting of a basic GridWorld designed to mimic the CliffWalking environment from
https://www.gymlibrary.dev/environments/toy_text/cliff_walking/
Performs episodes consisting of steps taken in the environment untill the termination conditino is reached.
The environment provides rewards from which a Policy is trained using an implementation of the Q-learning algorithm.


## 

## Instaling

can be installed through cabal using the 
> cabal install

## Running App

app can also be run through cabal using

> cabal run GridWorldRL

## Running Tests
tests can be run through cabal as well using

> cabal test

or 

>  cabal run GridWorldRL

