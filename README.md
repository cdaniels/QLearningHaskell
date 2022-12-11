### Functional Q-Learning in Haskell

A simple RL demonstration using Haskell
Consisting of a basic GridWorld designed to mimic the CliffWalking environment from
https://www.gymlibrary.dev/environments/toy_text/cliff_walking/
Performs episodes consisting of steps taken in the environment untill the termination conditino is reached.
The environment provides rewards from which a Policy is trained using an implementation of the Q-learning algorithm.
Data is collected from each episode and learning is performed across episodes untill the set episode number is reached.
Multiple learning runs are performed up to the set run number and data is averaged across the runs before being displayed as a line plot.


## Instaling

can be installed through cabal using the 

> cabal install

for plotting additional python dependencies are required. Thes can be installed using:

> sudo apt-get install -y python3 python3-pip python3-matplotlib python3-numpy python3-tk python-mpltoolkits.basemap python3-scipy dvipng


## Running App

app can also be run through cabal using

> cabal run GridWorldRL

## Running Tests
tests can be run through cabal as well using

> cabal test

or 

> cabal run GridWorldRL-test

