context("Testing activations")

###############################################
# Run code
###############################################

test_that("Test elu_R", {
  expect_true( all(elu_R(x = c(1, 2, 3)) == c(1, 2, 3)))
  expect_true( all(elu_R(x = c(-3, -2, -1)) == c(exp(-3) -1, exp(-2)-1, exp(-1)-1)))
  expect_true( all(elu_R(x = c(-3, -2, -1), 2) == 2*c(exp(-3) -1, exp(-2)-1, exp(-1)-1)))
})

test_that("Test softmax_R", {
  expect_true( all(softmax_R(x = c(1, 1, 1, 1)) == c(.25, .25, .25, .25)))
  expect_true( all(softmax_R(x = c(2, 2)) == c(.5, .5)))
})

test_that("Test softplus_R", {
  expect_true( all(softplus_R(x = c(1, 2)) == c(log(exp(1) + 1), log(exp(2) + 1))))
})

test_that("Test softsign_R", {
  expect_true( all(softsign_R(x = c(1, 3)) == c(.5, .75)))
  expect_true( all(softsign_R(x = c(-1, -3)) == c(-.5, -.75)))
  expect_true( all(softsign_R(x = c(-1, 3)) == c(-.5, .75)))
})

test_that("Test relu_R", {
  expect_true( all(relu_R(x = c(1, 2, 3)) == c(1, 2, 3)))
  expect_true( all(relu_R(x = c(-1, -2, -3)) == c(0, 0, 0)))
  expect_true( all(relu_R(x = c(1, 2, 3), max_value = -2) == c(-2, -2, -2)))
  expect_true( all(relu_R(x = c(-1, -2, -3), threshold = -10) == c(-1, -2, -3)))
})

test_that("Test tanh_R", {
  expect_true( all(tanh_R(x = c(1, 2, 3)) == tanh(1:3)))
})

test_that("Test sigmoid_R", {
  expect_true( all(sigmoid_R(x = c(0)) == .5))
})

test_that("Test hard_sigmoid_R", {
  expect_true( all(hard_sigmoid_R(x = c(-3, 0, 3)) == c(0, .5, 1)))
})

test_that("Test exponential_R", {
  expect_true( all(exponential_R(x = c(-3, 0, 3)) == c(exp(-3), exp(0), exp(3))))
})

test_that("Test linear_R", {
  expect_true( all(linear_R(x = c(-1, 0, 1)) == c(-1, 0, 1)))
})
