# grnnet - An R Implementation of Generalized Regression Neural Network

## About

**grnnet** is currently under active development.  

**grnnet** implements the *Generalized Regression Neural Network (GRNN)* 
architecture by Specht (1991).  

A GRNN is a variation of radial basis neural and can be related to nonparametric
regression. In its essence a GRNN can be seen as a formulation of the k-nearest 
neighbor algorithm in the terminology of neural networks, where each instance is
regarded as artificial neuron and its activation is given by a radial basis 
function or any other suitable kernel-like function.  

The current package allows to create single- and multi-output regression neural
network and produces heteroscedastic predictive interval using the conformal
prediction framework.  

A **grnnet** object is trained with an internal leave-one-out cross-validation
loop and statistically validated with K-fold cross-validation loop.

## TODO

- Implement conditional density estimation using this network architecture.

### References

Specht, D. F. (1991). A general regression neural network. IEEE Transactions on Neural Networks, 2(6), 568-576.
