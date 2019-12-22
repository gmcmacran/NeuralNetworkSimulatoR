context("simulate_regression_data")

###############################################
# Run code
###############################################
set.seed(1)

###################
# Linear Regression
###################

matrices = list(matrix(10, nrow = 1, ncol = 1))
activations = list(linear_R)

out <- simulate_regression_data(rows = 100L, N = 1L, U = 0L, C = 0L, matrices = matrices, activations = activations, noise = 0)

test_that("Linear Regression 1 variable", {
  expect_true( ncol(out) == 2)
  expect_true( all(round(out[,1] * 10, 8) == round(out[, 2], 8)))
})


matrices = list(matrix(1:3, nrow = 3, ncol = 1))
activations = list(linear_R)

out <- simulate_regression_data(rows = 100L, N = 3L, U = 0L, C = 0L, matrices = matrices, activations = activations, noise = 0)

test_that("Linear Regression 3 variables", {
  expect_true( ncol(out) == 4)
  expect_true( all(round(out[,1] * 1 + out[,2] * 2 + out[,3] * 3, 8) == round(out[, 4], 8)))
})

rm(matrices, activations)

###################
# Argument N works as expected
###################
numVar <- 1L
matrices = list(matrix(1:numVar, nrow = numVar, ncol = 1))
activations = list(linear_R)

out <- simulate_regression_data(rows = 10L, N = numVar, U = 0L, C = 0L, matrices = matrices, activations = activations, noise = 0)

test_that("1 normal variable", {
  expect_true( ncol(out) == numVar + 1)
})

numVar <- 3L
matrices = list(matrix(1:numVar, nrow = numVar, ncol = 1))
activations = list(linear_R)

out <- simulate_regression_data(rows = 10L, N = numVar, U = 0L, C = 0L, matrices = matrices, activations = activations, noise = 0)

test_that("1 normal variable", {
  expect_true( ncol(out) == numVar + 1)
})

numVar <- 5L
matrices = list(matrix(1:numVar, nrow = numVar, ncol = 1))
activations = list(linear_R)

out <- simulate_regression_data(rows = 10L, N = numVar, U = 0L, C = 0L, matrices = matrices, activations = activations, noise = 0)

test_that("1 normal variable", {
  expect_true( ncol(out) == numVar + 1)
})

rm(numVar, matrices, activations)

###################
# Argument U works as expected
###################
numVar <- 1L
matrices = list(matrix(1:numVar, nrow = numVar, ncol = 1))
activations = list(linear_R)

out <- simulate_regression_data(rows = 10L, N = 0L, U = numVar, C = 0L, matrices = matrices, activations = activations, noise = 0)

test_that("1 normal variable", {
  expect_true( ncol(out) == numVar + 1)
})

numVar <- 3L
matrices = list(matrix(1:numVar, nrow = numVar, ncol = 1))
activations = list(linear_R)

out <- simulate_regression_data(rows = 10L, N = 0L, U = numVar, C = 0L, matrices = matrices, activations = activations, noise = 0)

test_that("1 normal variable", {
  expect_true( ncol(out) == numVar + 1)
})

numVar <- 5L
matrices = list(matrix(1:numVar, nrow = numVar, ncol = 1))
activations = list(linear_R)

out <- simulate_regression_data(rows = 10L, N = 0L, U = numVar, C = 0L, matrices = matrices, activations = activations, noise = 0)

test_that("1 normal variable", {
  expect_true( ncol(out) == numVar + 1)
})

rm(numVar, matrices, activations)

###################
# Argument C works as expected
###################
numVar <- 1L
matrices = list(matrix(1:numVar, nrow = numVar, ncol = 1))
activations = list(linear_R)

out <- simulate_regression_data(rows = 10L, N = 0L, U = 0L, C = numVar, matrices = matrices, activations = activations, noise = 0)

test_that("1 normal variable", {
  expect_true( ncol(out) == numVar + 1)
})

numVar <- 3L
matrices = list(matrix(1:numVar, nrow = numVar, ncol = 1))
activations = list(linear_R)

out <- simulate_regression_data(rows = 10L, N = 0L, U = 0L, C = numVar, matrices = matrices, activations = activations, noise = 0)

test_that("1 normal variable", {
  expect_true( ncol(out) == numVar + 1)
})

numVar <- 5L
matrices = list(matrix(1:numVar, nrow = numVar, ncol = 1))
activations = list(linear_R)

out <- simulate_regression_data(rows = 10L, N = 0L, U = 0L, C = numVar, matrices = matrices, activations = activations, noise = 0)

test_that("1 normal variable", {
  expect_true( ncol(out) == numVar + 1)
})

rm(numVar, matrices, activations)

###################
# Argument noise works as expected
###################

matrices = list(matrix(1, nrow = 1, ncol = 1))
activations = list(linear_R)

out <- simulate_regression_data(rows = 100L, N = 1L, U = 0L, C = 0L, matrices = matrices, activations = activations, noise = 0)
out2 <- simulate_regression_data(rows = 100L, N = 1L, U = 0L, C = 0L, matrices = matrices, activations = activations, noise = 10)
test_that("1 normal variable", {
  expect_true( sd(out[,2]) < sd(out2[,2]))
})

rm(matrices, activations)

###############################################
# Input checking
###############################################

matrices = list(matrix(1, nrow = 1, ncol = 1))
activations = list(linear_R)

test_that("Confirm rows checing works.", {
  expect_error(simulate_regression_data(rows = c(100, 200),
                                        N = 1L, U = 0L, C = 0L,
                                        matrices = matrices, activations = activations, noise = 0),
               NULL)
  expect_error(simulate_regression_data(rows = 100,
                                        N = 1L, U = 0L, C = 0L,
                                        matrices = matrices, activations = activations, noise = 0),
               NULL)
  expect_error(simulate_regression_data(rows = -10L,
                                        N = 1L, U = 0L, C = 0L,
                                        matrices = matrices, activations = activations, noise = 0),
               NULL)
})

test_that("Confirm N checing works.", {
  expect_error(simulate_regression_data(rows = 100L,
                                        N = c(1L, 2L),  U = 0L, C = 0L,
                                        matrices = matrices, activations = activations, noise = 0),
               NULL)
  expect_error(simulate_regression_data(rows = 100L,
                                        N = 1, U = 0L, C = 0L,
                                        matrices = matrices, activations = activations, noise = 0),
               NULL)
  expect_error(simulate_regression_data(rows = 100L,
                                        N = -1L, U = 0L, C = 0L,
                                        matrices = matrices, activations = activations, noise = 0),
               NULL)
})

test_that("Confirm U checing works.", {
  expect_error(simulate_regression_data(rows = 100L,
                                        N = 0L,  U = c(1L, 2L), C = 0L,
                                        matrices = matrices, activations = activations, noise = 0),
               NULL)
  expect_error(simulate_regression_data(rows = 100L,
                                        N = 0L, U = 1, C = 0L,
                                        matrices = matrices, activations = activations, noise = 0),
               NULL)
  expect_error(simulate_regression_data(rows = 100L,
                                        N = 0L, U = -1L, C = 0L,
                                        matrices = matrices, activations = activations, noise = 0),
               NULL)
})

test_that("Confirm C checing works.", {
  expect_error(simulate_regression_data(rows = 100L,
                                        N = 0L,  U = 0L, C = c(1L, 2L),
                                        matrices = matrices, activations = activations, noise = 0),
               NULL)
  expect_error(simulate_regression_data(rows = 100L,
                                        N = 0L, U = 0L, C = 1,
                                        matrices = matrices, activations = activations, noise = 0),
               NULL)
  expect_error(simulate_regression_data(rows = 100L,
                                        N = 0L, U = 0L, C = -1L,
                                        matrices = matrices, activations = activations, noise = 0),
               NULL)
})

test_that("Confirm matrices checing works.", {
  expect_error(simulate_regression_data(rows = 100L,
                                        N = 1L,  U = 0L, C = 0L,
                                        matrices = list(),
                                        activations = activations, noise = 0),
               NULL)
  expect_error(simulate_regression_data(rows = 100L,
                                        N = 1L, U = 0L, C = 0L,
                                        matrices = list(matrix(1, nrow = 1, ncol = 1), matrix(1, nrow = 1, ncol = 1)),
                                        activations = activations,
                                        noise = 0),
               NULL)
  expect_error(simulate_regression_data(rows = 100L,
                                        N = 1L, U = 0L, C = 0L,
                                        matrices = list(matrix(1:3, nrow = 3, ncol = 1)),
                                        activations = activations, noise = 0),
               NULL)
})

test_that("Confirm activations checing works.", {
  expect_error(simulate_regression_data(rows = 100L,
                                        N = 1L,  U = 0L, C = 0L,
                                        matrices = matrices,
                                        activations = list(), noise = 0),
               NULL)
  expect_error(simulate_regression_data(rows = 100L,
                                        N = 1L,  U = 0L, C = 0L,
                                        matrices = matrices,
                                        activations = list(linear_R, linear_R), noise = 0),
               NULL)
})
