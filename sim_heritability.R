
library(AlphaSimR)



FOUNDERPOP <- runMacs(nInd = 100,
                      nChr = 1,
                      inbred = FALSE,
                      species = "GENERIC")
 
 
## Simulation parameters
 
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
ped$dam[ped$dam == 0] <- NA
ped$sire[ped$sire == 0] <- NA



write.csv(pheno,
          file = "sim_pheno.csv",
          row.names = FALSE,
          quote = FALSE)

write.csv(ped,
          file = "sim_ped.csv",
          row.names = FALSE,
          quote = FALSE)
