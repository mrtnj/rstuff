
library(AlphaSimR)
library(dplyr)
library(patchwork)
library(purrr)

founders <- runMacs(nInd = 100,
                    nChr = 30)

simparam <- SimParam$new(founders)

simparam$addTraitAD(nQtlPerChr = 10,
                    meanDD = 0.25,
                    varDD = 0.5)
simparam$setVarE(h2 = 0.3)
                    

generations_up <- vector(length = 20,
                         mode = "list")

generations_down <- vector(length = 20,
                           mode = "list")


generations_up[[1]] <- newPop(founders,
                              simParam = simparam)

generations_down[[1]] <- newPop(founders,
                                simParam = simparam)


for (gen_ix in 2:20) {
  generations_up[[gen_ix]] <- selectCross(pop = generations_up[[gen_ix -1 ]],
                                          nInd = 20,
                                          nCrosses = 50,
                                          simParam = simparam)
  
  generations_down[[gen_ix]] <- selectCross(pop = generations_down[[gen_ix -1 ]],
                                          nInd = 20,
                                          nCrosses = 50,
                                          selectTop = FALSE,
                                          simParam = simparam)
}


## Dot plots

get_gv <- function(generation) {
  data.frame(id = generation@id,
             value = generation@gv[,1],
             stringsAsFactors = FALSE)
}


get_stats <- function(generations) {
  data.frame(generation = 1:length(generations),
             mean = unlist(lapply(generations, meanG)),
             sd = sqrt(unlist(lapply(generations, varG))))
}

stats_up <- get_stats(generations_up)
stats_down <- get_stats(generations_down)

stats <- rbind(transform(stats_up,
                         selection = "up",
                         stringsAsFactors = FALSE),
               transform(stats_down,
                         selection = "down",
                         stringsAsFactors = FALSE))

stats$selection[stats$generation == 1] <- "base population"

gv_up <- map_dfr(generations_up, get_gv, .id = "generation")
gv_down <- map_dfr(generations_down, get_gv, .id = "generation")


gv <- rbind(transform(gv_up,
                      selection = "up",
                      stringsAsFactors = FALSE),
            transform(gv_down,
                      selection = "down",
                      stringsAsFactors = FALSE))

gv <- filter(gv, generation != 1)

gv <- rbind(gv,
            transform(filter(gv_up, generation == 1),
                      selection = "base population"))

plot_dots <- ggplot() +
  geom_quasirandom(aes(x = generation,
                       y = value,
                       colour = selection),
                       data = gv,
                   alpha = I(0.1)) +
  geom_ribbon(aes(x = generation,
                  ymin = mean - sd,
                  ymax = mean + sd,
                  fill = selection),
              data = stats,
              alpha = I(0.2)) +
  geom_line(aes(x = generation,
                y = mean,
                colour = selection),
            data = stats) +
  scale_x_discrete(limits = 1:20) +
  scale_colour_manual(values = c("black", "red", "blue")) +
  scale_fill_manual(values = c("black", "red", "blue")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  ylab("Additive genetic value") +
  ggtitle("Change in genetic mean")


## Heatmaps

gv_split <- split(gv, list(gv$generation))

gv_pseudo_id <- map_dfr(gv_split,
                        function(x) {
                          if (nrow(x) > 0) {
                            x$pseudo_id <- 1:nrow(x)
                          }
                          x
                        })



plot_heatmap <- qplot(x = generation, y = pseudo_id, fill = value, geom = "tile", data = gv_pseudo_id) +
  scale_x_discrete(limits = 1:20) +
  scale_fill_gradient2() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  ylab("Individual") +
  ggtitle("Additive genetic value of individuals")



## QTN plot

effects <- simparam$traits[[1]]@addEff

get_f <- function(generations) {
  geno <- lapply(generations, pullQtlGeno, simParam = simparam)
  
  f <- lapply(geno, function(x) colSums(x)/2/nrow(x))
  
  map_dfr(f, function(x) data.frame(qtl_id = 1:length(x),
                                    f = x,
                                    effect = effects,
                                    stringsAsFactors = FALSE),
          .id = "generation")
}

f_up <- get_f(generations_up)
f_down <- get_f(generations_down)

f <- rbind(transform(f_up,
                     selection = "up"),
           transform(f_down,
                     selection = "down"))


plot_qtn <- qplot(x = generation, y = f,
                  data = filter(f, abs(effect) > 0.15),
                  group = qtl_id,
                  geom = "line",
                  colour = effect) +
  scale_x_discrete(limits = 1:20) +
  scale_color_gradient2() +
  facet_wrap(~ selection, ncol = 1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  ylab("Allele frequency") +
  ggtitle("Frequency of large-effect alleles in selection lines")
  


plot_combined <- plot_dots /
  plot_heatmap /
  plot_qtn 


pdf("selection_heatmaps.pdf",
    width = 8.3,
    height = 11.7)
print(plot_combined)
dev.off()
