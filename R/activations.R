elu_R <- function(x, alpha = 1) {
  out <- ifelse(x > 0,
    x,
    alpha * (exp(x) - 1)
  )

  return(out)
}

softmax_R <- function(x) {
  out <- exp(x)
  out <- out / sum(out)

  return(out)
}

softplus_R <- function(x) {
  out <- log(exp(x) + 1)

  return(out)
}

softsign_R <- function(x) {
  out <- x / (abs(x) + 1)

  return(out)
}

relu_R <- function(x, alpha = 0, max_value = Inf, threshold = 0) {
  out <- ifelse(x >= threshold, x, alpha * (x - threshold))
  out <- pmin(out, max_value)

  return(out)
}

tanh_R <- function(x) {
  out <- tanh(x)

  return(out)
}

sigmoid_R <- function(x) {
  out <- 1 / (1 + exp(-x))

  return(out)
}

hard_sigmoid_R <- function(x) {
  out <- ifelse(x < -2.5, 0, ifelse(x > 2.5, 1, .2 * x + .5))

  return(out)
}

exponential_R <- function(x) {
  out <- exp(x)

  return(out)
}

linear_R <- function(x) {
  out <- identity(x)

  return(out)
}
