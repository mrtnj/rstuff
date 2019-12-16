library(AGHmatrix)
library(AlphaSimR)
library(egg)
library(ggplot2)
library(tidyr)
 
## Simulate data

FOUNDERPOP <- runMacs(nInd = 100,
                      nChr = 20,
                      inbred = FALSE,
                      species = "GENERIC")

SIMPARAM <- SimParam$new(FOUNDERPOP)
SIMPARAM$addTraitA(nQtlPerChr = 100,
                   mean = 100,
                   var = 10)
SIMPARAM$setGender("yes_sys")
SIMPARAM$setVarE(h2 = 0.3)
 
generations <- vector(mode = "list", length = 10) 
generations[[1]] <- newPop(FOUNDERPOP,
                           simParam = SIMPARAM)
 
for (gen in 2:10) {
 
    generations[[gen]] <- randCross(generations[[gen - 1]],
                                    nCrosses = 10,
                                    nProgeny = 10,
                                    simParam = SIMPARAM)
 
}
 
combined <- Reduce(c, generations)
 
 
pheno <- data.frame(animal = combined@id,
                    pheno = combined@pheno[,1])
 
ped <- data.frame(id = combined@id,
                  dam = combined@mother,
                  sire =combined@father)

## A slice of pedigree matrix

A <- Amatrix(ped)

A_slice <- A[901:100, 901:100]

A_slice_df <- as.data.frame(A_slice)
A_slice_df$id1 <- rownames(A_slice_df)

A_long <- pivot_longer(A_slice_df, -id1, names_to = "id2")


plot_A  <- qplot(x = id1, y = id2, fill = value, geom = "tile", data = A_long) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none") +
    scale_fill_gradient(low = "white", high = "blue") +
    ggtitle("A matrix") +
    xlab("") +
    ylab("")


## Its Cholesky decomposition

L <- chol(A_slice)

L_df <- as.data.frame(L)
L_df$id1 <- rownames(A_slice_df)

L_long <- pivot_longer(L_df, -id1, names_to = "id2")


plot_L  <- qplot(x = id1, y = id2, fill = value, geom = "tile", data = L_long) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none") +
    scale_fill_gradient2(mid = "white", low = "red", high = "blue") +
    ggtitle("L matrix") +
    xlab("") +
    ylab("")


plot_combined  <- ggarrange(plot_A, plot_L, ncol = 2)


pdf("A_L_matrices.pdf", width = 6, height = 3)
print(plot_combined)
dev.off()
