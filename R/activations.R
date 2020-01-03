#' @title Elu Activation Function in R
#'
#' @param x A numeric vector.
#' @param alpha A numeric scalar.
#' @export
elu_R <- function(x, alpha = 1) {
  out <- ifelse(x > 0,
    x,
    alpha * (exp(x) - 1)
  )

  return(out)
}

#' @title Softmax Activation Function in R
#'
#' @param x A numeric vector.
#' @export
softmax_R <- function(x) {
  out <- exp(x)
  out <- out / sum(out)

  return(out)
}

#' @title Softplus Activation Function in R
#'
#' @param x A numeric vector.
#' @export
softplus_R <- function(x) {
  out <- log(exp(x) + 1)

  return(out)
}

#' @title Softsign Activation Function in R
#'
#' @param x A numeric vector.
#' @export
softsign_R <- function(x) {
  out <- x / (abs(x) + 1)

  return(out)
}

#' @title Relu Activation Function in R
#'
#' @param x A numeric vector.
#' @param alpha A numeric scalar.
#' @param max_value A numeric scalar.
#' @param threshold A numeric scalar.
#' @export
relu_R <- function(x, alpha = 0, max_value = Inf, threshold = 0) {
  out <- ifelse(x >= threshold, x, alpha * (x - threshold))
  out <- pmin(out, max_value)

  return(out)
}

#' @title Tanh Activation Function in R
#'
#' @param x A numeric vector.
#' @export
tanh_R <- function(x) {
  out <- tanh(x)

  return(out)
}

#' @title Sigmoid Activation Function in R
#'
#' @param x A numeric vector.
#' @export
sigmoid_R <- function(x) {
  out <- 1 / (1 + exp(-x))

  return(out)
}

#' @title Hard Sigmoid Activation Function in R
#'
#' @param x A numeric vector.
#' @export
hard_sigmoid_R <- function(x) {
  out <- ifelse(x < -2.5, 0, ifelse(x > 2.5, 1, .2 * x + .5))

  return(out)
}

#' @title Exponential Activation Function in R
#'
#' @param x A numeric vector.
#' @export
exponential_R <- function(x) {
  out <- exp(x)

  return(out)
}

#' @title Linear Activation Function in R
#'
#' @param x A numeric vector.
#' @export
linear_R <- function(x) {
  out <- identity(x)

  return(out)
}
