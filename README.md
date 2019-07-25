# financial-contagion-in-R
This folder rebuild the model in the paper: [Contagion in Financial Networks](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=1577043) by Prasanna Gai.

## Create Network
This script can create certain networks and initialize it to let it suitable for analyses.
It can build:

1. ER network;
2. BA network;
3. SBM network under the condition that p<sub>cc</sub> = p<sub>cp</sub>;
4. SBM network under the condition that p<sub>pp</sub> = p<sub>cp</sub>;
5. Build from the file.

The initialization of a network will give the edges a weight, depends on the number of nieghbors and the setting of interbank liabilities.

## Judge Bankrupt
This script can test the nodes whether bankrupt. There have two types of bankrupt:

1. Directly bankrupt due to the point neighbor (the neighbor have links point to this node) bankrupt;

2. Indirectly bankrupt that recieve liabilities from more than one bankrupt banks that exceed the capital buffer.

## Simultate Bankrupt
This scipt contains functions to simulate the bankrupt with one randomly initally credit event. The initial bankrupt point will be randomly chosen or choose the biggest connectted bank.

The loops is stopped when there is no more bank bankrupt in the system.

## Plot the Figure
This script contains the functions to make simple plot, or the raw code to plot the beautiful figure.

## Other Names is the Network Models
These scripts will do the test on a certain type of network models to do some various analyses.
The results are shown below here:

<img src="https://github.com/Rewove/financial-contagion-in-R/blob/master/contagion%20example%20figures/Final_ER.png" width=450 height=450/>

<img src="https://github.com/Rewove/financial-contagion-in-R/blob/master/contagion%20example%20figures/ER_big_win.png" width=450 height=450/>

<img src="https://github.com/Rewove/financial-contagion-in-R/blob/master/contagion%20example%20figures/target.png" width=450 height=450/>

<img src="https://github.com/Rewove/financial-contagion-in-R/blob/master/contagion%20example%20figures/SBM%20pp%3Dcp.png" width=1000 height=700/>

<img src="https://github.com/Rewove/financial-contagion-in-R/blob/master/contagion%20example%20figures/SBM%20cc%3Dcp.png" width=1000 height=660/>


