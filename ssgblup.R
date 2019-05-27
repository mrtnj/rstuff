library(AGHmatrix)
library(AlphaSimR)
library(egg)
library(ggplot2)
library(reshape2)


## Set up population

FOUNDERPOP <- runMacs(nInd = 100,
                      nChr = 1,
                      segSites = 600,
                      inbred = FALSE,
                      species = "GENERIC")



SIMPARAM <- SimParam$new(FOUNDERPOP)
SIMPARAM$addTraitA(nQtlPerChr = 100,
                   mean = 100,
                   var = 10)
SIMPARAM$addSnpChip(nSnpPerChr = 500)
SIMPARAM$setGender("yes_sys")



pop <- newPop(FOUNDERPOP,
              simParam = SIMPARAM)


breeding <- vector(length = 5, mode = "list")
breeding[[1]] <- pop

for (i in 2:5) {
    print(i)

    breeding[[i]] <- randCross(breeding[[i - 1]],
                               nCrosses = 10,
                               nProgeny = 10,
                               simParam = SIMPARAM)
    breeding[[i]] <-  setPheno(breeding[[i]],
                               varE = 20,
                               simParam = SIMPARAM)
}



final_population <- Reduce(c, breeding)

pedigree <- data.frame(id = final_population@id,
                       sire = final_population@father,
                       dam = final_population@mother,
                       stringsAsFactors = FALSE)


geno <- pullSnpGeno(final_population, simParam = SIMPARAM)
geno22 <- geno[401:500,]


g_raw <- Gmatrix(geno22)

a <- Amatrix(pedigree)


## Break down A

a11 <- a[1:400, 1:400]
a12 <- a[1:400, 401:500]
a21 <- a[401:500, 1:400]
a22 <- a[401:500, 401:500]



## G adjustment

a_adj <- mean(a22) - mean(g_raw)
b_adj <- 1 - a_adj/2

g <- a_adj + b_adj * g_raw



## H

a22inv <- solve(a22)

h11 <- a11 - a12 %*% a22inv %*% a21 + a12 %*% a22inv %*% g %*% a22inv %*% a21
h12 <- a12 %*% a22inv %*% g
h21 <- g %*% a22inv %*% a21

h <- rbind(cbind(h11, h12),
           cbind(h21, g))


## Plotting

labels <- list(xlab("ID"), ylab("ID"), theme_bw())

gmelted <- melt(g)
amelted <- melt(a)
a22melted <- melt(a22)
hmelted <- melt(h)

plot_g <- qplot(x = Var1, y = Var2, fill = value, data = gmelted, geom = "tile") +
    labels
plot_a <- qplot(x = Var1, y = Var2, fill = value, data = amelted, geom = "tile") +
    labels
plot_a22 <- qplot(x = Var1, y = Var2, fill = value, data = a22melted, geom = "tile") +
    labels
plot_h <- qplot(x = Var1, y = Var2, fill = value, data = hmelted, geom = "tile") +
    labels


plot_a22_g <- ggarrange(plot_a22 + ggtitle("Pedigree relationship"),
                        plot_g + ggtitle("Genomic relationship"), ncol = 2)


delta <- a - h
deltamelted <- melt(delta)
plot_delta_h <- qplot(x = Var1, y = Var2, fill = value, data = deltamelted, geom = "tile") +
    scale_fill_gradient2() +
    geom_vline(xintercept = 400) +
    geom_hline(yintercept = 400) +
    labels



