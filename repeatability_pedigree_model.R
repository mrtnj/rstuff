
## Simulate some data and demonstrate a pedigree-based animal model with repeated records in hglm

library(AlphaSimR)
library(AGHmatrix)
library(ggplot2)
library(hglm)
library(patchwork)
library(purrr)
library(tibble)



## Simulate founder individuals
founders <- runMacs(nInd = 50, nChr = 10)


## Simulate a two-generation pedigree with traits with given variance, based on those founders

run_simulation <- function(founders, var_g, var_e, var_ind,
                           n_crosses, n_progeny, n_records) {

  simparam <- SimParam$new(founders)
  
  simparam$addTraitA(nQtlPerChr = 100, var = var_g)
  simparam$setSexes("yes_sys")

  pop <- newPop(founders, simParam = simparam)
  
  n_gen <- 2
  
  generations <- vector(length = n_gen + 1,
                        mode = "list")
  
  generations[[1]] <- pop
  
  for (gen_ix in 2:(n_gen + 1)) {
    
    generations[[gen_ix]] <- randCross(generations[[gen_ix - 1]],
                                       nCrosses = n_crosses,
                                       nProgeny = n_progeny, 
                                       simParam = simparam)
    
  }
  
  combined <- Reduce(c, generations)
  
  
  ped <- data.frame(id = combined@id,
                    father = combined@father,
                    mother = combined@mother,
                    stringsAsFactors = FALSE)
  
  ind_effect <- rnorm(generations[[3]]@nInd, 0, sqrt(var_ind))
  
  pheno <- pmap_dfr(list(id = generations[[3]]@id, gv = generations[[3]]@gv[,1], ind_effect = ind_effect),
                    function(id, gv, ind_effect) tibble(id = rep(id, n_records),
                                                        pheno = rep(gv + ind_effect, n_records)))
  
  pheno$pheno <- pheno$pheno + rnorm(nrow(pheno), 0, sqrt(var_e))
  
  
  list(ped = ped,
       pheno = pheno,
       animals = generations[[3]])

} 



## Helper functions for modelling

## Create incidence matrix for individual observations

get_Z_rep <- function(pheno, ind_start_ix) {
  
  n_ind <- length(unique(pheno$id))
  n_obs <- nrow(pheno)
  
  Z_rep <- matrix(0, ncol = n_ind, nrow = n_obs)
  
  for (obs_ix in 1:nrow(pheno)) {
    
    ind_id <- as.numeric(pheno$id[obs_ix])
    Z_rep[obs_ix, ind_id - ind_start_ix] <- 1
    
  }
  Z_rep
}

## Get genetic incidence matrix

get_Z_ped <- function(ped, pheno, ind_start_ix) {
  
  n_ind <- length(unique(pheno$id))
  n_obs <- nrow(pheno)
  
  A <- Amatrix(ped)

  A3 <- A[ind_start_ix:nrow(ped), ind_start_ix:nrow(ped)]
  
  ind_ix <- as.numeric(pheno$id) - ind_start_ix + 1
  
  L <- t(chol(A3))

  Z_ped <- L[ind_ix, ]
  
  Z_ped
}


## Modelling functions

## A model with just an individual random effect

fit_repeatability_model <- function(sim) {
  ped <- sim$ped
  pheno <- sim$pheno
  
  n_ind <- length(unique(pheno$id))
  
  ind_start_ix <- nrow(ped) - n_ind + 1
  
  X <- model.matrix(~1, pheno)
  
  Z_rep <- get_Z_rep(pheno, ind_start_ix)
  
  model <- hglm(y = pheno$pheno,
                X = X,
                Z = Z_rep,
                conv = 1e-8)
  
  model
}


## A model with an individual random effect and an additive genetic effect

fit_repeatability_pedigree_model <- function(sim) {
  ped <- sim$ped
  pheno <- sim$pheno
  
  n_ind <- length(unique(pheno$id))
  
  ind_start_ix <- nrow(ped) - n_ind + 1
  
  Z_ped <- get_Z_ped(ped, pheno, ind_start_ix)
  
  X <- model.matrix(~1, pheno)
  
  Z_rep <- get_Z_rep(pheno, ind_start_ix)
  
  model <- hglm(y = pheno$pheno,
                X = X,
                Z = cbind(Z_rep, Z_ped),
                RandC = c(ncol(Z_rep), ncol(Z_ped)),
                conv = 1e-8,
                maxit = 100)
  
  model
}


## Functions to extract results from models


## Estimates from repeatability model

get_variance_estimates_repeatability <- function(model) {
  var_ranef <- model$varRanef
  se_var_ind <- exp(model$SummVC2[[1]][2])
  se_var_e <- exp(model$SummVC1[2])
  tibble(var_ind = var_ranef,
         se_var_ind = se_var_ind,
         var_e = model$varFix,
         se_var_e = se_var_e)
  
}


## Estimates from model with repeatability and additive genetic value

get_variance_estimates_pedigree <- function(model) {
  var_ranef <- model$varRanef
  se_var_ind <- exp(model$SummVC2[[1]][2])
  se_var_g <- exp(model$SummVC2[[2]][2])
  se_var_e <- exp(model$SummVC1[2])
  tibble(var_ind = var_ranef[1],
         se_var_ind = se_var_ind,
         var_g = var_ranef[2],
         se_var_g = se_var_g,
         var_e = model$varFix,
         se_var_e = se_var_e)
  
}




## Run simulation in ten replicates

sims <- map(1:10, function(x) run_simulation(founders,
                                             var_g = 2,
                                             var_e = 3,
                                             var_ind = 1,
                                             n_crosses = 10,
                                             n_progeny = 10,
                                             n_records = 10))

## Fit models to replicates

repeatability_models <- map(sims, fit_repeatability_model)

pedigree_models <- map(sims, fit_repeatability_pedigree_model)  



## Get estimates

ests_rep <- map_dfr(repeatability_models, get_variance_estimates_repeatability,
                    .id = "rep")

ests_ped <- map_dfr(pedigree_models, get_variance_estimates_pedigree,
                    .id = "rep")



## Plot the estimates


## Repeatability only

plot_var_ind_rep <- qplot(x = rep,
                          y = var_ind,
                          ymin = var_ind - 2 * se_var_ind,
                          ymax = var_ind + 2 * se_var_ind,
                          data = ests_rep,
                          geom = "pointrange") +
  geom_hline(yintercept = 0, linetype = 2, colour = "blue")



plot_var_e_rep <- qplot(x = rep,
                        y = var_e,
                        ymin = var_e - 2 * se_var_e,
                        ymax = var_e + 2 * se_var_e,
                        data = ests_rep,
                        geom = "pointrange") +
  geom_hline(yintercept = 0, linetype = 2, colour = "blue")


plot_rep <- plot_var_ind_rep / plot_var_e_rep



## Animal model

plot_var_ind <- qplot(x = rep,
                      y = var_ind,
                      ymin = var_ind - 2 * se_var_ind,
                      ymax = var_ind + 2 * se_var_ind,
                      data = ests_ped,
                      geom = "pointrange") +
  geom_hline(yintercept = 0, linetype = 2, colour = "blue")


plot_var_g <- qplot(x = rep,
                    y = var_g,
                    ymin = var_g - 2 * se_var_g,
                    ymax = var_g + 2 * se_var_g,
                    data = ests_ped,
                    geom = "pointrange") +
  geom_hline(yintercept = 0, linetype = 2, colour = "blue")


plot_var_e <- qplot(x = rep,
                    y = var_e,
                    ymin = var_e - 2 * se_var_e,
                    ymax = var_e + 2 * se_var_e,
                    data = ests_ped,
                    geom = "pointrange") +
  geom_hline(yintercept = 0, linetype = 2, colour = "blue")


plot_ped <- plot_var_ind / plot_var_g / plot_var_e
