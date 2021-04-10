library(AlphaSimR)
library(broom)
library(dplyr)
library(ggplot2)
library(purrr)

primitive_gwas <- function(training) {
  
  geno <- as.data.frame(pullSnpGeno(training))
  
  models <- lapply(geno, function(g) lm(g ~ training@pheno[,1]))
  
  estimates <- map_dfr(models, tidy, .id = "marker")
  filter(estimates, term != "(Intercept)")
}


sim_pop <- function(x) {
  founderpop <- runMacs2(nInd = 2000,
                         nChr = 10,
                         segSites = 1100,
                         Ne = 1000,
                         histNe = NULL,
                         histGen = NULL)
  
  SP <- SimParam$new(founderpop)
  SP$addTraitA(1000)
  SP$addSnpChip(100)
  SP$setVarE(h2 = 0.5)
  SP$setGender("yes_sys")
  SP$restrSegSites(maxQtl = 1000,
                   maxSnp = 100,
                   overlap = FALSE)
  
  
  pop <- newPop(founderpop)
  
  training <- pop[1:1000]
  
  f1_close <- randCross(training, nCrosses = 100, nProgeny = 2)
  f1_distant <- randCross(pop[1001:2000], nCrosses = 100, nProgeny = 2)
  
  ebv <- RRBLUP(training, simParam = SP)
  
  gwas <- primitive_gwas(training)
  
  f1_close <- setEBV(pop = f1_close, solution = ebv)
  f1_distant <- setEBV(pop = f1_distant, solution = ebv)
  
  score_close <- pullSnpGeno(f1_close) %*% gwas$estimate
  score_distant <- pullSnpGeno(f1_distant) %*% gwas$estimate
  
  sib1_ix <- seq(from = 1, to = 200, by = 2)
  sib2_ix <- seq(from = 2, to = 200, by = 2)
  
  cor_ebv_sibs_close <- cor(f1_close@ebv[sib1_ix],
                            f1_close@ebv[sib2_ix])
  cor_ebv_sibs_distant <- cor(f1_distant@ebv[sib1_ix],
                              f1_distant@ebv[sib2_ix])
  
  cor_score_sibs_close <- cor(f1_close@ebv[sib1_ix],
                              f1_close@ebv[sib2_ix])
  cor_score_sibs_distant <- cor(f1_distant@ebv[sib1_ix],
                                f1_distant@ebv[sib2_ix])
  
  cor_gv_sibs_close <- cor(f1_close@gv[sib1_ix],
                           f1_close@gv[sib2_ix])
  cor_gv_sibs_distant <- cor(f1_distant@gv[sib1_ix],
                              f1_distant@gv[sib2_ix])
  accuracy_ebv_close <- cor(f1_close@gv,
                        f1_close@ebv)
  accuracy_ebv_distant <- cor(f1_distant@gv,
                        f1_distant@ebv)
  
  accuracy_score_close <- cor(f1_close@gv,
                              score_close)
  accuracy_score_distant <- cor(f1_distant@gv,
                                score_distant)
  
  data.frame(cor_ebv_sibs_close,
             cor_ebv_sibs_distant,
             cor_score_sibs_close,
             cor_score_sibs_distant,
             cor_gv_sibs_close,
             cor_gv_sibs_distant,
             accuracy_ebv_close,
             accuracy_ebv_distant,
             accuracy_score_close,
             accuracy_score_distant)
}


res <- map_dfr(1:50, sim_pop)

saveRDS(res,
        file = "corr_siblings_results.Rds")


## Summarise

res_summary <- map_dfr(res,
                       function(x) data.frame(average = mean(x),
                                              lower = quantile(x, 0.05),
                                              upper = quantile(x, 0.95)),
                       .id = "variable")


plot_cors <- qplot(x = variable,
                   y = average,
                   ymin = lower,
                   ymax = upper,
                   geom = "pointrange",
                   data = res_summary[grepl("cor_", res_summary$variable),]) +
  coord_flip()

plot_accuracy <- qplot(x = variable,
                       y = average,
                       ymin = lower,
                       ymax = upper,
                       geom = "pointrange",
                       data = res_summary[grepl("accuracy_", res_summary$variable),]) +
  coord_flip()





