
library(AGHmatrix)
library(AlphaSimR)
library(brms)
library(hglm)
library(rstan)


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

h2 <-  varG(generations[[1]])/varP(generations[[1]])
    
pheno <- data.frame(animal = combined@id,
                    pheno = combined@pheno[,1],
                    scaled_pheno = as.vector(scale(combined@pheno[,1])),
                    stringsAsFactors = FALSE)

ped <- data.frame(id = combined@id,
                  dam = combined@mother,
                  sire = combined@father,
                  stringsAsFactors = FALSE)

## hglm

A  <- Amatrix(ped)

Z0  <- diag(1000)

L <- t(chol(A))

Z  <- Z0 %*% L

X <- model.matrix(~1, pheno)

model <- hglm(y = pheno$pheno,
              X = model.matrix(~1, pheno),
              Z = Z,
              conv = 1e-8)

est_h2  <- model$varRanef / (model$varRanef + model$varFix)




## Stan

stan_model <- stan(file = "nishio_arakawa.stan",
                   data = list(Y = pheno$scaled_pheno,
                               X = X,
                               A = A,
                               Z = Z0,
                               J = 1,
                               K = 1000,
                               N = 1000))

est_h2_stan <- summary(stan_model, pars = "h2")$summary


## brms

model_brms <- brm(scaled_pheno ~ 1 + (1|animal),
                  data = pheno,
                  family = gaussian(),
                  cov_ranef = list(animal = A),
                  chains = 4,
                  cores = 1,
                  iter = 2000)

posterior_brms <- posterior_samples(model_brms, pars = c("sd_animal", "sigma"))

h2_brms  <- posterior_brms[,1]^2 / (posterior_brms[,1]^2 + posterior_brms[,2]^2)

est_h2_brms <- mean(h2_brms)
