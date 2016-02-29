
source("life_functions.R")

load("replicates.Rdata")

virus <- read_board("virus_30x30.txt")
blocks <- read_board("blocks_30x30.txt")
blank <- read_board("blank_30x30.txt")


still <- read_board("still_lifes.txt")

png("still_life.png")
display(still)
dev.off()

oscillators <- read_board("blinkers.txt")

animate_board(oscillators, 3, "oscillators.gif")

png("blocks.png")
display(blocks)
dev.off()

animate_board(virus, 80, "virus.gif")

png("blocks_trajectory_plot.png")
print(trajectory_plot(blocks_replicates[[1]]))
dev.off()

png("blocks_g10_1.png")
display(blocks_replicates[[1]][[10]]$board[[1]])
dev.off()

png("blocks_g10_80.png")
display(blocks_replicates[[1]][[10]]$board[[1]] %>% tick(80))
dev.off()


png("blocks_g10_1b.png")
display(blocks_replicates[[1]][[10]]$board[[90]])
dev.off()

png("blocks_g10_80b.png")
display(blocks_replicates[[1]][[10]]$board[[90]] %>% tick(80))
dev.off()


png("mean_fitness_blocks.png")
print(mean_fitness_plot(blocks_replicates) + ylim(0, 130))
dev.off()

png("mean_fitness_virus.png")
print(mean_fitness_plot(virus_replicates) + ylim(0, 130))
dev.off()

png("mean_fitness_blank.png")
print(mean_fitness_plot(blank_replicates))
dev.off()


animate_board(blank_replicates[[3]][[10]]$board[[4]], 80, "blank_denovo.gif")