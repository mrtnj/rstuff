
## Demonstrate use of Mendelian inconsistencies to exclude parents

library(AlphaSimR)
library(dplyr)
library(ggplot2)


founderpop <- runMacs(nInd = 105,
                      nChr = 10,
                      segSites = 100)

simparam <- SimParam$new(founderpop)
simparam$setGender("no")

simparam$addSnpChip(nSnpPerChr = 100)

parents <- newPop(founderpop,
                  simParam = simparam)

ewes <- parents[1:100]
rams <- parents[101:105]

lambs <- randCross2(females = ewes,
                    males = rams,
                    nCrosses = 100,
                    nProgeny = 2,
                    simParam = simparam)
                    


## Generate the possible gametes from a genotype

possible_gametes <- function(genotype) {
    
    if (genotype == 0) {
        gametes <- 0
    } else if (genotype == 1) {
        gametes <- c(0, 1)
    } else if (genotype == 2) {
        gametes <- 1
    }
       
    gametes
}

## Generate the possible genotypes for an offspring from parent possible gametes

possible_genotypes <- function(father_gametes,
                               mother_gametes) {
 
    possible_combinations <- expand.grid(father_gametes, mother_gametes)
    resulting_genotypes <- rowSums(possible_combinations)
    unique(resulting_genotypes)
}


## Check offspring genotypes for consistency with parent gentotypes

mendelian_inconsistency <- function(ewe,
                                  ram,
                                  lamb) {

    n_markers <- length(ewe)
    inconsistent <- logical(n_markers)

    for (marker_ix in 1:n_markers) {
        
        possible_lamb_genotypes <-
          possible_genotypes(possible_gametes(ewe[marker_ix]),
                             possible_gametes(ram[marker_ix]))
        
        inconsistent[marker_ix] <-
          !lamb[marker_ix] %in% possible_lamb_genotypes
    }
    
    sum(inconsistent)
}

opposite_homozygotes <- function(ram,
                                 lamb) {
    sum(lamb == 0 & ram == 2) + 
        sum(lamb == 2 & ram == 0)

}


## Now, we go through all the lambs, we take the mother as given,
## and check their consistency with all the potential rams

check_lambs <- function(ewes,
                        rams,
                        lambs) {

    rams_geno <- pullSnpGeno(rams,
                             simParam = simparam)
    
    ewes_geno <- pullSnpGeno(ewes,
                             simParam = simparam)
    
    lambs_geno <- pullSnpGeno(lambs,
                              simParam = simparam)


    inconsistent_with_rams <- vector(length = lambs@nInd,
                                     mode = "list")
    
    for (lamb_ix in 1:lambs@nInd) {
        
        lamb_geno <- lambs_geno[lamb_ix,]   
        ewe_geno <- ewes_geno[which(ewes@id == lambs@mother[lamb_ix]),]
        
        inconsistent_with_rams[[lamb_ix]] <-
            data.frame(lamb_id = lambs@id[lamb_ix],
                       ram_id = numeric(rams@nInd),
                       inconsistent_markers = numeric(rams@nInd),
                       opposite_homozygotes = numeric(rams@nInd))
        
        for (ram_ix in 1:rams@nInd) {
            ram_geno <- rams_geno[ram_ix,]
            inconsistent_with_rams[[lamb_ix]]$inconsistent_markers[ram_ix] <-
              mendelian_inconsistency(ewe_geno,
                                      ram_geno,
                                      lamb_geno)
            inconsistent_with_rams[[lamb_ix]]$opposite_homozygotes[ram_ix] <-
              opposite_homozygotes(ram_geno,
                                   lamb_geno)
            inconsistent_with_rams[[lamb_ix]]$ram_id[ram_ix] <- rams@id[ram_ix]
        }
    }
    
    Reduce(rbind, inconsistent_with_rams)
}


lambs_inconsistency <- check_lambs(ewes, rams, lambs)



## Plot of consistency

random_lambs <- sample(lambs@id, 40)

true_father <- data.frame(lamb_id = lambs@id,
                          ram_id = lambs@father)

plot_tile <- ggplot() +
  geom_tile(aes(x = lamb_id,
                y = ram_id,
                fill = (1000 - inconsistent_markers)/10),
            data = filter(lambs_inconsistency, lamb_id %in% random_lambs)) +
  geom_point(aes(x = lamb_id,
                 y = ram_id),
             colour = "red",
             data = filter(true_father, lamb_id %in% random_lambs)) +
  theme_bw() +
  scale_fill_gradient(name = "Percent consistent") +
  theme(legend.position = "bottom")


plot_methods <- qplot(x = opposite_homozygotes,
                      y = inconsistnt_markers,
                      data = lambs_inconsistency) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Comparison between methods") +
  xlab("Opposite homozygotes (ram--lamb)") +
  ylab("Inconsistent markers (including ewe)")


