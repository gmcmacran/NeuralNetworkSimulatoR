simulate_classification_data <- function(rows = 100, N = 5, U = 5, C = 0, matrices, activations) {
  #################
  # Check inputs
  #################
  if (length(rows) != 1) {
    stop("Argument rows should have length one.")
  }
  if (!is.integer(rows)) {
    stop("Argument rows should be an integer.")
  }
  if (rows < 0) {
    stop("Argument rows nonnegative.")
  }

  # Variable inputs
  if (length(N) != 1) {
    stop("Argument N should have length one.")
  }
  if (!is.integer(N)) {
    stop("Argument N should be an integer.")
  }
  if (N < 0) {
    stop("Argument N nonnegative.")
  }

  if (length(U) != 1) {
    stop("Argument U should have length one.")
  }
  if (!is.integer(U)) {
    stop("Argument U should be an integer.")
  }
  if (U < 0) {
    stop("Argument U nonnegative.")
  }

  if (length(C) != 1) {
    stop("Argument C should have length one.")
  }
  if (!is.integer(C)) {
    stop("Argument C should be an integer.")
  }
  if (C < 0) {
    stop("Argument C nonnegative.")
  }

  if (length(matrices) != length(activations)) {
    stop("Arguments matrices and activations should have the same length.")
  }

  if (length(matrices) < 1) {
    stop("Arguments matrices should have positive length.")
  }

  # Confrim matrices dimentions are correct.
  if (nrow(matrices[[1]]) != N + U + C) {
    stop("The number of rows in the first elemet of argument matrices did not equal N + U + C.")
  }

  if (length(matrices) > 1) {
    for (i in 2:length(matrices)) {
      tempCol <- dim(matrices[[i - 1]])[2]
      tempRow <- dim(matrices[[i]])[1]

      if (tempCol != tempRow) {
        stop(paste("Invalid dimensions between elements", i - 1, "and", i, "of argument matrices."))
      }
    }
  }

  if (ncol(matrices[[length(matrices)]]) != 1) {
    stop("The last matrix must have one column. Multiheaded networks not supported.")
  }

  #################
  # Make input data
  #################
  if (N > 0) {
    norms <- matrix(rnorm(rows * N), nrow = rows, ncol = N)
    colnames(norms) <- stringr::str_c(rep("N", N), 1:N)
  } else {
    norms <- matrix(0, nrow = rows, ncol = 0)
  }

  if (U > 0) {
    uniforms <- matrix(runif(rows * U), nrow = rows, ncol = U)
    colnames(uniforms) <- stringr::str_c(rep("U", U), 1:U)
  } else {
    uniforms <- matrix(0, nrow = rows, ncol = 0)
  }

  if (C > 0) {
    Cs <- matrix(ifelse(runif(rows * C) > .5, 1, 0),
      nrow = rows, ncol = C
    )
    colnames(Cs) <- stringr::str_c(rep("C", C), 1:C)
  } else {
    Cs <- matrix(0, nrow = rows, ncol = 0)
  }

  IN <- cbind(norms, uniforms, Cs)

  #################
  # Make response variable
  #################
  OUT <- IN

  for (i in 1:length(matrices)) {
    OUT <- OUT %*% matrices[[i]]
    OUT <- activations[[i]](OUT)
  }

  if (any(OUT > 1)) {
    stop("Invalid set of matrices and activations. Probability above 1.
         Using sigmoid_R or softmax_R as the last activation will probably fix this.")
  }
  if (any(OUT < 0)) {
    stop("Invalid set of matrices and activations. Probability below 0.
         Using sigmoid_R or softmax_R as the last activation will probably fix this.")
  }

  OUT <- matrix(stats::rbinom(nrow(OUT), 1, OUT), ncol = 1)
  colnames(OUT) <- "Response"

  OUT2 <- cbind(IN, OUT)
  colnames(OUT2) <- c(colnames(IN), colnames(OUT))

  return(OUT2)
}
