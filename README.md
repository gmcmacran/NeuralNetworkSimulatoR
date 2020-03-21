
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NeuralNetworkSimulatoR

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/NeuralNetworkSimulatoR)](https://cran.r-project.org/package=NeuralNetworkSimulatoR)
[![Travis build
status](https://travis-ci.org/gmcmacran/NeuralNetworkSimulatoR.svg?branch=master)](https://travis-ci.org/gmcmacran/NeuralNetworkSimulatoR)
[![Codecov test
coverage](https://codecov.io/gh/gmcmacran/NeuralNetworkSimulatoR/branch/master/graph/badge.svg)](https://codecov.io/gh/gmcmacran/NeuralNetworkSimulatoR?branch=master)
<!-- badges: end -->

Neural networks come with many settings. Learning rates, number of
nodes, number of layers, activation functions, drop out and many more.

With all these settings, what happens if the network is too deep? Or too
shallow? Or too wide? How can these questions be answered considering
all models are approximations?

The goal of this package is to create ideal data for a network
structure. If the data scientist knows how deep the correct network is,
they can assess what happens if the network is too shallow. Best
practices based on simulations can be discovered.

## Installation

You can install the released version of NeuralNetworkSimulatoR from
[CRAN](https://CRAN.R-project.org) with:

``` r
#Not on cran. Under active development.
install.packages("NeuralNetworkSimulatoR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gmcmacran/NeuralNetworkSimulatoR")
```

## Technical Details

This package does not train models. It only creates data. Therefore
there are no deep learning framework dependencies. The examples use
keras, but any framework could be used to train models.

Standard activation functions are implemented in this package. It can
also be extended to use custom activations that are implemented in R.

## Mathematical Background

A feed forward neural network structure is defined as a series of matrix
multiplications and activation functions. The values of the first hidden
layer are defined as

\[
A(M_1*M_2)
\]

where \(M_1\) and \(M_2\) are matrices and \(A\) is a activation
function. The elements of \(M_2\) are the parameters to be estimated
during model training.

Once all matrices and activation functions are defined, the network is
defined.

## Example: Linear Regression

The linear regression model is a simple network with no hidden layers.
First we must provide the matrix and activation function. Note the “1:3”
in M sets the true values to be estimated.

``` r
library(NeuralNetworkSimulatoR)

#Weights are 1, 2, and 3
M <- list(matrix(1:3, nrow = 3, ncol = 1))
A <- list(linear_R)
```

A data set with 1000 rows and 3 predictors following a normal
distribution is created.

``` r
set.seed(1)
simData <- simulate_regression_data(
  rows = 1000L,
  N = 3L, U = 0L, C = 0L,
  matrices = M, activations = A,
  noise = 0
)

head(simData)
#>              N1          N2          N3   Response
#> [1,] -0.6264538  1.13496509 -0.88614959 -1.0149724
#> [2,]  0.1836433  1.11193185 -1.92225490 -3.3592577
#> [3,] -0.8356286 -0.87077763  1.61970074  2.2819184
#> [4,]  1.5952808  0.21073159  0.51926990  3.5745537
#> [5,]  0.3295078  0.06939565 -0.05584993  0.3007493
#> [6,] -0.8204684 -1.66264885  0.69641761 -2.0565133
```

Lets train a model and see if the true weights are estimated accurately.

``` r
library(keras)

X <- simData[,1:3]
Y <- simData[,4]


model <- keras_model_sequential() %>%
  layer_dense(units = 1, 
              activation = "linear", 
              input_shape = 3,
              use_bias = FALSE, 
              kernel_initializer = initializer_constant(value = 1))

model %>%
  compile(loss = loss_mean_squared_error,
          optimizer = optimizer_rmsprop())

model %>%
  fit(x = X, y = Y,
      epochs = 30,
      batch_size = 10,
      verbose = FALSE)

get_weights(model)
#> [[1]]
#>           [,1]
#> [1,] 0.9998387
#> [2,] 2.0003510
#> [3,] 3.0011110
```

The true weights were estimated well. This is model fits the data nearly
perfectly.

With the correct model known, the data scientist can assess what happens
when the model is too deep.

## See vignettes for more complex use cases of this package.
