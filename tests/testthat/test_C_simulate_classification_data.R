context("Testing simulate_classification_data")

###############################################
# Run code
###############################################
set.seed(1)

###################
# logistic Regression
###################

matrices <- list(matrix(.1, nrow = 1, ncol = 1))
activations <- list(sigmoid_R)

out <- simulate_classification_data(rows = 100L, N = 1L, U = 0L, C = 0L, matrices = matrices, activations = activations)

test_that("Linear Regression 1 variable", {
  expect_true(ncol(out) == 2)
  expect_true(all(out[, 2] %in% c(0, 1)))
})


matrices <- list(matrix(1:3, nrow = 3, ncol = 1))
activations <- list(sigmoid_R)

out <- simulate_classification_data(rows = 100L, N = 3L, U = 0L, C = 0L, matrices = matrices, activations = activations)

test_that("Linear Regression 3 variables", {
  expect_true(ncol(out) == 4)
  expect_true(all(out[, 4] %in% c(0, 1)))
})

rm(matrices, activations)

###################
# Argument rows  works as expected
###################
matrices <- list(matrix(.1, nrow = 1, ncol = 1))
activations <- list(sigmoid_R)

test_that("1 normal variable", {
  expect_true(simulate_classification_data(
    rows = 10L,
    N = 1L, U = 0L, C = 0L,
    matrices = matrices, activations = activations
  )
  %>%
    nrow() == 10)
  expect_true(simulate_classification_data(
    rows = 100L,
    N = 1L, U = 0L, C = 0L,
    matrices = matrices, activations = activations
  )
  %>%
    nrow() == 100)
  expect_true(simulate_classification_data(
    rows = 200L,
    N = 1L, U = 0L, C = 0L,
    matrices = matrices, activations = activations
  )
  %>%
    nrow() == 200)
})

###################
# Argument N works as expected
###################
numVar <- 1L
matrices <- list(matrix(1:numVar, nrow = numVar, ncol = 1))
activations <- list(sigmoid_R)

out <- simulate_classification_data(rows = 10L, N = numVar, U = 0L, C = 0L, matrices = matrices, activations = activations)

test_that("1 normal variable", {
  expect_true(ncol(out) == numVar + 1)
})

numVar <- 3L
matrices <- list(matrix(1:numVar, nrow = numVar, ncol = 1))
activations <- list(sigmoid_R)

out <- simulate_classification_data(rows = 10L, N = numVar, U = 0L, C = 0L, matrices = matrices, activations = activations)

test_that("1 normal variable", {
  expect_true(ncol(out) == numVar + 1)
})

numVar <- 5L
matrices <- list(matrix(1:numVar, nrow = numVar, ncol = 1))
activations <- list(sigmoid_R)

out <- simulate_classification_data(rows = 10L, N = numVar, U = 0L, C = 0L, matrices = matrices, activations = activations)

test_that("1 normal variable", {
  expect_true(ncol(out) == numVar + 1)
})

rm(numVar, matrices, activations)

###################
# Argument U works as expected
###################
numVar <- 1L
matrices <- list(matrix(1:numVar, nrow = numVar, ncol = 1))
activations <- list(sigmoid_R)

out <- simulate_classification_data(rows = 10L, N = 0L, U = numVar, C = 0L, matrices = matrices, activations = activations)

test_that("1 normal variable", {
  expect_true(ncol(out) == numVar + 1)
})

numVar <- 3L
matrices <- list(matrix(1:numVar, nrow = numVar, ncol = 1))
activations <- list(sigmoid_R)

out <- simulate_classification_data(rows = 10L, N = 0L, U = numVar, C = 0L, matrices = matrices, activations = activations)

test_that("1 normal variable", {
  expect_true(ncol(out) == numVar + 1)
})

numVar <- 5L
matrices <- list(matrix(1:numVar, nrow = numVar, ncol = 1))
activations <- list(sigmoid_R)

out <- simulate_classification_data(rows = 10L, N = 0L, U = numVar, C = 0L, matrices = matrices, activations = activations)

test_that("1 normal variable", {
  expect_true(ncol(out) == numVar + 1)
})

rm(numVar, matrices, activations)

###################
# Argument C works as expected
###################
numVar <- 1L
matrices <- list(matrix(1:numVar, nrow = numVar, ncol = 1))
activations <- list(sigmoid_R)

out <- simulate_classification_data(rows = 10L, N = 0L, U = 0L, C = numVar, matrices = matrices, activations = activations)

test_that("1 normal variable", {
  expect_true(ncol(out) == numVar + 1)
})

numVar <- 3L
matrices <- list(matrix(1:numVar, nrow = numVar, ncol = 1))
activations <- list(sigmoid_R)

out <- simulate_classification_data(rows = 10L, N = 0L, U = 0L, C = numVar, matrices = matrices, activations = activations)

test_that("1 normal variable", {
  expect_true(ncol(out) == numVar + 1)
})

numVar <- 5L
matrices <- list(matrix(1:numVar, nrow = numVar, ncol = 1))
activations <- list(sigmoid_R)

out <- simulate_classification_data(rows = 10L, N = 0L, U = 0L, C = numVar, matrices = matrices, activations = activations)

test_that("1 normal variable", {
  expect_true(ncol(out) == numVar + 1)
})

rm(numVar, matrices, activations)

###############################################
# Input checking
###############################################

matrices <- list(matrix(1, nrow = 1, ncol = 1))
activations <- list(sigmoid_R)

test_that("Confirm rows checking works.", {
  expect_error(
    simulate_classification_data(
      rows = c(100, 200),
      N = 1L, U = 0L, C = 0L,
      matrices = matrices, activations = activations
    ),
    NULL
  )
  expect_error(
    simulate_classification_data(
      rows = 100,
      N = 1L, U = 0L, C = 0L,
      matrices = matrices, activations = activations
    ),
    NULL
  )
  expect_error(
    simulate_classification_data(
      rows = -10L,
      N = 1L, U = 0L, C = 0L,
      matrices = matrices, activations = activations
    ),
    NULL
  )
})

test_that("Confirm N checking works.", {
  expect_error(
    simulate_classification_data(
      rows = 100L,
      N = c(1L, 2L), U = 0L, C = 0L,
      matrices = matrices, activations = activations
    ),
    NULL
  )
  expect_error(
    simulate_classification_data(
      rows = 100L,
      N = 1, U = 0L, C = 0L,
      matrices = matrices, activations = activations
    ),
    NULL
  )
  expect_error(
    simulate_classification_data(
      rows = 100L,
      N = -1L, U = 0L, C = 0L,
      matrices = matrices, activations = activations
    ),
    NULL
  )
})

test_that("Confirm U checking works.", {
  expect_error(
    simulate_classification_data(
      rows = 100L,
      N = 0L, U = c(1L, 2L), C = 0L,
      matrices = matrices, activations = activations
    ),
    NULL
  )
  expect_error(
    simulate_classification_data(
      rows = 100L,
      N = 0L, U = 1, C = 0L,
      matrices = matrices, activations = activations
    ),
    NULL
  )
  expect_error(
    simulate_classification_data(
      rows = 100L,
      N = 0L, U = -1L, C = 0L,
      matrices = matrices, activations = activations
    ),
    NULL
  )
})

test_that("Confirm C checking works.", {
  expect_error(
    simulate_classification_data(
      rows = 100L,
      N = 0L, U = 0L, C = c(1L, 2L),
      matrices = matrices, activations = activations
    ),
    NULL
  )
  expect_error(
    simulate_classification_data(
      rows = 100L,
      N = 0L, U = 0L, C = 1,
      matrices = matrices, activations = activations
    ),
    NULL
  )
  expect_error(
    simulate_classification_data(
      rows = 100L,
      N = 0L, U = 0L, C = -1L,
      matrices = matrices, activations = activations
    ),
    NULL
  )
})

test_that("Confirm matrices checking works.", {
  expect_error(
    simulate_classification_data(
      rows = 100L,
      N = 1L, U = 0L, C = 0L,
      matrices = list(),
      activations = activations
    ),
    NULL
  )
  expect_error(
    simulate_classification_data(
      rows = 100L,
      N = 1L, U = 0L, C = 0L,
      matrices = list(matrix(1, nrow = 1, ncol = 1), matrix(1, nrow = 1, ncol = 1)),
      activations = activations
    ),
    NULL
  )
  expect_error(
    simulate_classification_data(
      rows = 100L,
      N = 1L, U = 0L, C = 0L,
      matrices = list(matrix(1:3, nrow = 3, ncol = 1)),
      activations = activations
    ),
    NULL
  )
  expect_error(
    simulate_classification_data(
      rows = 100L,
      N = 3L, U = 0L, C = 0L,
      matrices = list(matrix(1:3, nrow = 3, ncol = 2)),
      activations = activations
    ),
    NULL
  )
})

test_that("Confirm activations checking works.", {
  expect_error(
    simulate_classification_data(
      rows = 100L,
      N = 1L, U = 0L, C = 0L,
      matrices = matrices,
      activations = list()
    ),
    NULL
  )
  expect_error(
    simulate_classification_data(
      rows = 100L,
      N = 1L, U = 0L, C = 0L,
      matrices = matrices,
      activations = list(sigmoid_R, sigmoid_R)
    ),
    NULL
  )
  expect_error(
    simulate_classification_data(
      rows = 100L,
      N = 1L, U = 0L, C = 0L,
      matrices = matrices,
      activations = list(exponential_R)
    ),
    NULL
  )
})
