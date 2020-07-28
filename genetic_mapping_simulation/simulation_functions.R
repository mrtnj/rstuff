

## Create founder population

make_founders <- function(n,
                          split_generations,
                          inbred) {

    founders  <- runMacs(nInd = 2 * n,
                         nChr = 20,
                         split = split_generations,
                         inbred = inbred)

    simparam = SimParam$new(founders)
    simparam$addTraitA(nQtlPerChr = 100)
    simparam$addSnpChip(nSnpPerChr = 1000)
    simparam$addSnpChip(nSnpPerChr = 10)
    
    list(founders = newPop(founders,
                           simParam = simparam),
         simparam = simparam)
    
}


make_cross <- function(founders1,
                       founders2,
                       f1_crosses,
                       offspring,
                       crossing_generations,
                       mainentance_crosses,
                       ail_expansion_crosses) {

    f1 <- randomCross2(founders1,
                       founders2,
                       nCrosses = f1_crosses,
                       nProgeny = offspring)
    
    
}



library(AlphaSimR)
library(ggplot2)

founders <- make_founders(20, 100, FALSE)
geno <- pullSnpGeno(founders$founders, 1, simParam = founders$simparam)
pca <- prcomp(geno)
pc_data <- data.frame(pca$x[, 1:2],
                      population = rep(c(1, 2), each = 20))
plot_pcs <- qplot(x = PC1, y = PC2, colour = population, data = pc_data)
