source("life_functions.R")

virus <- read_board("virus_30x30.txt")
blocks <- read_board("blocks_30x30.txt")
blank <- read_board("blank_30x30.txt")

blocks_replicates <- replicate(5, evolve(blocks), simplify = FALSE)
blank_replicates <- replicate(5, evolve(blank), simplify = FALSE)
virus_replicates <- replicate(5, evolve(virus), simplify = FALSE)

save(blocks_replicates, blank_replicates, virus_replicates, file = "replicates.Rdata")