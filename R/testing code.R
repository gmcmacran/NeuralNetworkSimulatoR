set.seed(1)
rows = 1000L
N = 5L
U = 5L
C = 0L

matrices = list(matrix(1, nrow = N + U + C, ncol = 3), matrix(1, nrow = 3, ncol = 1))
activations = list(identity, identity)

temp <- simulate_data(rows, N, U, C,
                      list(matrix(1, nrow = N + U + C, ncol = 3), matrix(1, nrow = 3, ncol = 1)),
                      activations = list(identity, identity))


head(rowSums(temp[,1:10]))
head(temp[,11])


set.seed(1)
rows = 10L
N = 5L
U = 5L
C = 0L

matrices = list(matrix(1, nrow = N + U + C, ncol = 1))
activations = list(identity)

temp <- simulate_data(rows, N, U, C,
                      matrices,
                      activations)


head(rowSums(temp[,1:10]))
head(temp[,11])

