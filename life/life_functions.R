library(magrittr)
library(animation)
library(ggplot2)

## Read a board from tab-separated file. Return board matrix.
read_board <- function(filename) {
  as.matrix(read.table(filename, sep = "\t"))
}


## Develop one tick. Return new board matrix.
develop <- function(board_matrix) {
  padded <- rbind(matrix(0, nrow = 1, ncol = ncol(board_matrix) + 2),
                  cbind(matrix(0, ncol = 1, nrow = nrow(board_matrix)), 
                        board_matrix,
                        matrix(0, ncol = 1, nrow = nrow(board_matrix))),
                  matrix(0, nrow = 1, ncol = ncol(board_matrix) + 2))
  new_board <- padded
  for (i in 2:(nrow(padded) - 1)) {
    for (j in 2:(ncol(padded) - 1)) {
      neighbours <- sum(padded[(i-1):(i+1), (j-1):(j+1)]) - padded[i, j]
      if (neighbours < 2 | neighbours > 3) {
        new_board[i, j] <- 0
      }
      if (neighbours == 3) {
        new_board[i, j] <- 1
      }
    }
  }
  new_board[2:(nrow(padded) - 1), 2:(ncol(padded) - 1)]
}


## Develop a board a given number of ticks.
tick <- function(board_matrix, ticks) {
  if (ticks > 0) {
    for (i in 1:ticks) {
      board_matrix <- develop(board_matrix) 
    }
  }
  board_matrix
}


## Mutate a board
mutate <- function(board_matrix, mutation_rate) {
  mutated <- as.vector(board_matrix)
  outcomes <- rbinom(n = length(mutated), size = 1, prob = mutation_rate)
  for (i in 1:length(outcomes)) {
    if (outcomes[i] == 1)
      mutated[i] <- ifelse(mutated[i] == 0, 1, 0)
  }
  matrix(mutated, ncol = ncol(board_matrix), nrow = nrow(board_matrix))
}


## Display a board.
display <- function(board_matrix) {
  image(t(board_matrix)[, nrow(board_matrix):1])
}


## Animate a board as gif.

ani.options(interval = 0.25)

animate_board <- function(board_matrix, ticks, filename) {
  saveGIF(for (i in 0:ticks) display(tick(board_matrix, i)),
          movie.name = filename)  
}


## Calculates the fitness of an individual at a given time
get_fitness <- function(board_matrix, time) {
  board_matrix %>% tick(time) %>% sum
}


## Develop a generation and calculate fitness
grow <- function(generation) {
  generation$fitness <- sapply(generation$board, get_fitness, time = 80)
  generation
}



## Select a generation based on fitness, and create the next generation,
## adding mutation.
next_generation <- function(generation) {
  keep <- order(generation$fitness, decreasing = TRUE)[1:50]
  new_generation <- list(board = vector(mode = "list", length = 100),
                         fitness = numeric(100))
  ix <- rep(keep, each = 2)
  for (i in 1:100) new_generation$board[[i]] <- generation$board[[ix[i]]]
  new_generation$board <- lapply(new_generation$board, mutate, mutation_rate = mu)
  new_generation
}


## Evolve a board, with mutation and selection for a number of generation.
evolve <- function(board, n_gen = 10) { 
  generations <- vector(mode = "list", length = n_gen)

  generations[[1]] <- list(board = vector(mode = "list", length = 100),
                           fitness = numeric(100))
  for (i in 1:100) generations[[1]]$board[[i]] <- board
  generations[[1]]$board <- lapply(generations[[1]]$board, mutate, mutation_rate = mu)

  for (i in 1:(n_gen - 1)) {
    generations[[i]] <- grow(generations[[i]])
    generations[[i + 1]] <- next_generation(generations[[i]])
  }
  generations[[n_gen]] <- grow(generations[[n_gen]])
  generations
}

## Plot of fitness of individuals over generations.
trajectory_plot <- function(generations) {
  f <- unlist(lapply(generations, function(x) x$fitness))
  trajectory <- data.frame(individual = 1:(length(generations) * 100),
                           fitness = f, 
                           generation = rep(1:length(generations), each = 100))

  qplot(x = individual, y = fitness, data = trajectory, size = I(0.5)) +
    geom_vline(aes(xintercept = generation * 100)) + theme_bw() +
    xlab("Individual") + ylab("Fitness")
}


## Plot of average fitness in a replicate.
mean_fitness_plot <- function(replicates) {
  f <- unlist(sapply(replicates, function (replicate)
    sapply(replicate, function(generation) mean(generation$fitness)), simplify = FALSE))
  means <- data.frame(average_fitness = f,
                      replicate = factor(rep(1:length(replicates), each = length(replicates[[1]]))),
                      generation = rep(1:length(replicates[[1]]), length(replicates)))
  
  qplot(x = generation, y = average_fitness, data = means, colour = replicate, geom = "line") +
    theme_bw() + xlab("Generation") + ylab("Average fitness") +
    scale_x_continuous(breaks = 1:length(replicates[[1]]))
}


## Mutation rate
mu <- 0.0011