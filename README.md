# BBSsize

This project has two goals:

1) Re-generate the individual size data for the Breeding Bird Survey following the method in https://onlinelibrary.wiley.com/doi/full/10.1111/j.1466-8238.2010.00576.x
2) Extend the method to cover as much of the time series as possible (Thibault et al used 2003-2007, to generate a snapshot)

It is set up as a MATSS compendium (github.com/weecology/MATSS), because basically all of the computation involved is performing one operation on many many site-level datasets.

It will generate a new MATSS-shaped dataset.

It will need either external data storage (as the target output is the BBS data, but bigger) or for the user to be set up with the MATSS + retriever infrastructure.

I am not sure about permissions/liscensing for generating a new dataset like this, so I think the safe starting point is to make this repository be for the *code* to go from the BBS data you get from MATSS/retriever, to a version with indivdiual sizes added. Seed will matter.


