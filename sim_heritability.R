
library(AlphaSimR)
library(AnimalINLA)
library(ggplot2)
library(MCMCglmm)


results  <- vector(mode = "list",
                   length = 10)

resulting_h2  <- numeric(10)

for (sim_ix in 1:10) {

    FOUNDERPOP <- runMacs(nInd = 100,
                          nChr = 20,
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

    resulting_h2[sim_ix]  <-  varG(generations[[1]])/varP(generations[[1]])
    
    pheno <- data.frame(animal = combined@id,
                        pheno = combined@pheno[,1],
                        stringsAsFactors = FALSE)
    
    ped <- data.frame(id = combined@id,
                      dam = combined@mother,
                      sire =combined@father,
                      stringsAsFactors = FALSE)
    ped$dam[ped$dam == 0] <- NA
    ped$sire[ped$sire == 0] <- NA

    rm(generations)
    rm(combined)
    
    
    ##write.csv(pheno,
    ##          file = "sim_pheno.csv",
    ##          row.names = FALSE,
    ##          quote = FALSE)
    
    ##write.csv(ped,
    ##          file = "sim_ped.csv",
    ##          row.names = FALSE,
    ##          quote = FALSE)
    
    
    
    pheno$scaled  <- scale(pheno$pheno)
    

    ##MCMCglmm
    
    prior_gamma <- list(R = list(V = 1, nu = 1),
                        G = list(G1 = list(V = 1, nu = 1)))
    
    model_mcmc  <- MCMCglmm(scaled ~ 1,
                            random = ~ animal,
                            family = "gaussian",
                            prior = prior_gamma,
                            pedigree = ped,
                            data = pheno,
                            nitt = 100000,
                            burnin = 10000,
                            thin = 10)
    
    h2_mcmc_object  <- model_mcmc$VCV[, "animal"]/(model_mcmc$VCV[, "animal"] + model_mcmc$VCV[, "units"])
    
    h2_mcmc  <- data.frame(mean = mean(h2_mcmc_object),
                           lower = quantile(h2_mcmc_object, 0.025),
                           upper = quantile(h2_mcmc_object, 0.975),
                           method = "MCMC",
                           stringsAsFactors = FALSE)
    

    rm(model_mcmc)
    

    ## INLA

    
    ped_inla <- ped
    ped_inla$id  <- as.numeric(ped_inla$id)
    ped_inla$dam  <- as.numeric(ped_inla$dam)
    ped_inla$dam[is.na(ped_inla$dam)] <- 0
    ped_inla$sire  <- as.numeric(ped_inla$sire)
    ped_inla$sire[is.na(ped_inla$sire)] <- 0

    
    A_inv <- compute.Ainverse(ped_inla)
    
    
    model_inla  <- animal.inla(response = scaled,
                               genetic = "animal",
                               Ainverse = A_inv,
                               type.data = "gaussian",
                               data = pheno,
                               verbose = TRUE)
    
    summary_inla  <- summary(model_inla)
    
    h2_inla  <- data.frame(mean = summary_inla$summary.hyperparam["Heritability", "mean"],
                           lower = summary_inla$summary.hyperparam["Heritability", "0.025quant"],
                           upper = summary_inla$summary.hyperparam["Heritability", "0.975quant"],
                           method = "INLA",
                           stringsAsFactors = FALSE)

    rm(model_inla)
    
    
    results[[sim_ix]] <- rbind(h2_mcmc,
                                h2_inla)

    results[[sim_ix]]$rep <- sim_ix

}

results_combined  <- Reduce(rbind, results)

h2  <- data.frame(rep = 1:10,
                  h2 = resulting_h2)

plot_estimates  <- ggplot() + 
    geom_pointrange(aes(x = factor(rep), y = mean, ymin = lower, ymax = upper, colour = method),
                    data = results_combined,
                    position = position_dodge(0.5)) +
    geom_point(aes(x = factor(rep),
                   y = h2),
               data = h2) +
    xlab("Replicate") +
    ylab("Estimate") +
    coord_flip() +
    theme_bw() +
    theme(panel.grid = element_blank()) 
