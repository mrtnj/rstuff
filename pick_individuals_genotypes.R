
## Given a larger set of genotyped individuals, pick individuals that are informative for local mapping
## based on a number of chosen loci


library(AlphaSimR)
library(dplyr)
library(ggplot2)
library(purrr)
library(tibble)


## Simulate some SNP data to work with

founders <- runMacs(nInd = 400,
                    nChr = 10)

simparam <- SimParam$new(founders)

simparam$addSnpChip(nSnpPerChr = 100)

pop <- newPop(founders, simParam = simparam)

geno <- pullSnpGeno(pop, simParam = simparam)


f <- colSums(geno)/nrow(geno)/2

target_loci <- sample((1:ncol(geno))[f > 0.2 & f < 0.8], 3)


###########################################################################


## Functions to evaluate subsets based on how many homozygotes they have,
## and how balanced the two homozygotes are

prop_homozygotes <- function(g) {
  H <- sum(g == 1)/length(g)
  
  1 - H
}

balance_of_homozygotes <- function(g) {
  P <- sum(g == 0)/length(g)
  Q <- sum(g == 2)/length(g)
  
  1 - (P - Q)^2
}


h <- map_dbl(as.data.frame(geno), prop_homozygotes)

b <- map_dbl(as.data.frame(geno), balance_of_homozygotes)



## Calculate the product of proportion homozygotes and balance of homozygotes
## and return a sum of logarithms for all loci

evaluate_subset <- function(geno) {
  
  prop_h <- map_dbl(as.data.frame(geno), prop_homozygotes)
  balance <- map_dbl(as.data.frame(geno), balance_of_homozygotes)
  
  log_prod <- log10(prop_h * balance)
  
  sum(log_prod)
}

## Try it on all segregating sites

geno_seg <- geno[, colSums(geno) < 2 * nrow(geno) & colSums(geno) > 0]

score_subset <- evaluate_subset(geno_seg)


## Try it on random subsets

random_subset <- function(geno, size) {
  stopifnot(size > 1)
  sample(1:nrow(geno), size)
}

random_subsets <- map(1:100, function(x) random_subset(geno[, target_loci], 100))

scores <- map_dbl(random_subsets, function(s) evaluate_subset(geno[s, target_loci]))


max <- which.max(scores)
random_subsets[max]


lapply(as.data.frame(geno[random_subsets[[max]], target_loci]), table)


##############################################################################

## Functions to refine subsets with a genetic algorithm


## Mutate a subset by swapping in a number of individuals from the full population

mutate_subset <- function(subset, n_mutations, whole_population) {
  
  ## Exclude individuals already in subset
  to_sample_from <- setdiff(whole_population, subset)
  
  if (n_mutations > 0) {
    to_mutate_ix <- sample(1:length(subset), n_mutations)
    replace_with <- sample(to_sample_from, n_mutations)
    
    subset <- c(subset[-to_mutate_ix],
                replace_with)
                     
    
  }

  subset  
}


recombine_subset <- function(subset1, n_rec, subset2) {
  
  ## Exclude individuals already in subset
  to_sample_from <- setdiff(subset2, subset1)
  n_available <- length(to_sample_from)
  
  if (n_rec > n_available) {
    n_rec <- n_available
  }
  
  if (n_rec > 0 & n_available > 0) {
    to_swap_ix <- sample(1:length(subset), n_rec)
    replace_with <- sample(to_sample_from, n_rec)
    
    subset1 <- c(subset1[-to_swap_ix],
                 replace_with)
  }
    
  subset1
}



## Iterate

run_iterations <- function(geno, starting_subsets, n_it, n_mut, n_rec) {

  scores <- vector(length = n_it,
                   mode = "list")
  
  subsets <- vector(length = n_it,
                    mode = "list")
  
  subsets[[1]] <- starting_subsets
  scores[[1]] <- map_dbl(starting_subsets, function(s) evaluate_subset(geno[s, target_loci]))


  for (it_ix in 2:n_it) {
    ## Pick the best to save
    best <- subsets[[it_ix - 1]][which.max(scores[[it_ix - 1]])]
    
    ## Select
    subsets[[it_ix]] <- rep(subsets[[it_ix -1]][order(scores[[it_ix - 1]], decreasing = TRUE)[1:10]], each = 10)
    
    ## Mutate
    subsets[[it_ix]] <- map(subsets[[it_ix]],
                            mutate_subset,
                            n_mut = n_mut,
                            whole_population = 1:nrow(geno))
    
    ## Recombine
    to_recombine_with <- sample(1:length(subsets[[it_ix]]))
    subsets[[it_ix]] <- pmap(list(subset1 = subsets[[it_ix]],
                                  n_rec = n_rec,
                                  subset2 = to_recombine_with),
                             recombine_subset)
    
    subsets[[it_ix]] <- c(subsets[[it_ix]], best)
    
    ## Score
    scores[[it_ix]] <- map_dbl(subsets[[it_ix]],
                               function(s) evaluate_subset(geno[s, target_loci]))
  }

  list(subsets = subsets,
       scores = scores)  
}  


## Try on random subsets

iterations_random <- run_iterations(geno = geno,
                                    n_mut = 10,
                                    n_rec = 20,
                                    n_it = 50,
                                    starting_subsets = random_subsets)
  
lapply(as.data.frame(geno[iterations_random$subsets[[50]][[1]], target_loci]), table)

para_combinations <- expand.grid(n_mut = c(10, 20, 30, 40, 50),
                                 n_rec = c(10, 20, 30, 40, 50))
para_combinations$run <- as.character(1:nrow(para_combinations))

iterations_random_opt <- pmap(list(n_mut = para_combinations$n_mut,
                                   n_rec = para_combinations$n_rec),
                              function(n_mut, n_rec)
                                run_iterations(geno = geno, n_it = 50, starting_subsets = random_subsets,
                                               n_mut = n_mut, n_rec = n_rec))

get_best_scores <- function(iterations) {
  tibble(it = 1:length(iterations$score),
         best_score = unlist(lapply(iterations$score, max)),
         mean_score = unlist(lapply(iterations$score, mean)))
}



random_opt_scores <- map_dfr(iterations_random_opt, get_best_scores, .id = "run")

random_opt_scores_par <- inner_join(random_opt_scores, para_combinations)

plot_random_opt_scores <- qplot(x = it, y = best_score, group = run,
                                data = random_opt_scores_par,
                                geom = "line", colour = n_mut) +
  facet_wrap(~ n_rec)

##########################################################################

## Create a subset based on simple sampling


n_loci <- length(target_loci)

n_to_sample <- 100

n_per_homozygote <- floor(100/n_loci/2)

locus_sampled <- vector(length = n_loci,
                        mode = "list")

not_used_yet <- 1:nrow(geno)

for (locus_ix in 1:length(target_loci)) {

  locus_geno <- geno[not_used_yet, target_loci[locus_ix]]
  
  g0 <- which(locus_geno == 0)
  g2 <- which(locus_geno == 2)
  
  to_sample_g0 <- ifelse(length(g0) <= n_per_homozygote, length(g0), n_per_homozygote)
  to_sample_g2 <- ifelse(length(g2) <= n_per_homozygote, length(g2), n_per_homozygote)
  
  sampled_g0_ix <- sample(1:length(g0), to_sample_g0)
  sampled_g2_ix <- sample(1:length(g2), to_sample_g2)
  
  locus_sampled[[locus_ix]] <- c(not_used_yet[g0[sampled_g0_ix]],
                                 not_used_yet[g2[sampled_g2_ix]])  
  
  not_used_yet <- setdiff(not_used_yet, locus_sampled[[locus_ix]])
}

n_extras_needed <- n_to_sample - length(unlist(locus_sampled))

if (n_extras_needed > 0) {
  extras_ix <- sample(1:length(not_used_yet), n_extras_needed) 
  extras <- not_used_yet[extras_ix]
}

final_subset <- c(unlist(locus_sampled), extras)



lapply(as.data.frame(geno[final_subset, target_loci]), table)
evaluate_subset(geno[final_subset, target_loci])




## Expand with iterations

iterations_manual <- run_iterations(geno = geno, n_it = 50, starting_subsets = rep(list(final_subset), each = 100),
                                    n_mut = 10, n_rec = 20)

lapply(as.data.frame(geno[iterations_manual$subsets[[50]][[which.max(iterations_manual$scores[[50]])]], target_loci]), table)

iterations_manual$scores[[50]][which.max(iterations_manual$scores[[50]])]




iterations_manual_opt <- pmap(list(n_mut = para_combinations$n_mut,
                                   n_rec = para_combinations$n_rec),
                              function(n_mut, n_rec)
                                run_iterations(geno = geno, n_it = 50,
                                               starting_subsets = rep(list(final_subset), each = 100),
                                               n_mut = n_mut, n_rec = n_rec))


manual_opt_scores <- map_dfr(iterations_manual_opt, get_best_scores, .id = "run")

manual_opt_scores_par <- inner_join(manual_opt_scores, para_combinations)

plot_manual_opt_scores <- qplot(x = it, y = best_score, group = run,
                                data = manual_opt_scores_par,
                                geom = "line", colour = n_mut) +
  facet_wrap(~ n_rec)


