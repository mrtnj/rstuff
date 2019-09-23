library(MCMCglmm)
library(readr)


ped <- read_csv("sim_ped.csv")
pheno <- read_csv("sim_pheno.csv")


pheno$scaled <- scale(pheno$pheno)

prior_gamma <- list(R = list(V = 1, nu = 1),
                    G = list(G1 = list(V = 1, nu = 1)))


pheno <- as.data.frame(pheno)

model <- MCMCglmm(scaled ~ 1,
                  random = ~ animal,
                  family = "gaussian",
                  prior = prior_gamma,
                  pedigree = ped,
                  data = pheno,
                  nitt = 100000,
                  burnin = 10000,
                  thin = 10)
