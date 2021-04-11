
## Attempt to create a cutesy GWAS animation

library(dplyr)
library(gganimate)
library(ggplot2)
library(magick)
library(qtl)
library(tidyr)


## Read cross file

cross <- read.cross(format = "csv",
                    file = "41598_2016_BFsrep34031_MOESM83_ESM.csv")


## Extract individuals with missing covariates
cross <- subset(cross, ind = c("-34336", "-34233"))

## Setup covariates
pheno <- pull.pheno(cross)

covar <- model.matrix(~ sex_number + batch + PC1 + PC2 + PC3 + PC4 +
                      PC5 + PC6 + PC7 + PC8 + PC9 + PC10,
                      pheno,
                      na.action = na.exclude)[,-1]

## Single QTL scan
scan <- scanone(cross = cross,
                pheno.col = "weight_212_days",
                method = "hk",
                chr = 1,
                addcovar = covar)


## Get informative markers and combine with phenotypes for plotting

geno <- as.data.frame(pull.geno(cross,
                                chr = 1))

geno_values <- lapply(geno, unique)
informative <- unlist(lapply(geno_values, function(g) all(g %in% c(1:3, NA))))

geno_informative <- geno[informative]

geno_informative$id <- pheno$id
geno_informative$w212 <- pheno$weight_212_days
geno_informative$sex <- pheno$sex_number


## Scan plot
formatting <- list(theme_bw(base_size = 16),
                   theme(panel.grid = element_blank(),
                         strip.background = element_blank(),
                         legend.position = "none"),
                   scale_colour_manual(values = c("red", "purple", "blue")))

lod <- as.data.frame(scan)
lod <- lod[informative,]
lod$marker_number <- 1:nrow(lod)

plot_lod <- qplot(x = pos, y = lod, data = lod, geom = c("point", "line")) +
  ylab("Logarithm of odds") +
  xlab("Position") +
  formatting +
  transition_manual(marker_number,
                    cumulative = TRUE)

plot_lod_static <- qplot(x = pos, y = lod, data = lod, geom = c("point", "line")) +
  ylab("Logarithm of odds") +
  xlab("Position") +
  formatting 

png("lod_static.png", width = 640, height = 320)
print(plot_lod_static)
dev.off()


## Combined genotypes and weight plot

melted <- pivot_longer(geno_informative,
                       -c("id", "w212", "sex"))

melted <- na.exclude(melted)


marker_numbers <- data.frame(name = rownames(scan),
                             marker_number = 1:nrow(scan),
                             stringsAsFactors = FALSE)

melted <- inner_join(melted, marker_numbers)
melted$sex_char <- ifelse(melted$sex == 1, "male", "female")

plot_scatter <- qplot(x = value,
                     geom = "jitter",
                     y = w212,
                     colour = factor(value),
                     data = melted) +
  facet_wrap(~ factor(sex_char),
             ncol = 1) +
  xlab("Genotype") +
  ylab("Body mass") +
  formatting +
  transition_manual(marker_number)


plot_scatter_maxlod <- qplot(x = value,
                             geom = "jitter",
                             y = w212,
                             colour = factor(value),
                             data = filter(melted, name == "1_36652477")) +
  facet_wrap(~ factor(sex_char),
             ncol = 1) +
  xlab("Genotype") +
  ylab("Body mass") +
  formatting

png("lod_scatter.png", width = 640, height = 320)
print(plot_scatter_maxlod)
dev.off()


## Combine to gif

gif_lod <- animate(plot_lod,
                   fps = 2,
                   width = 320,
                   height = 320,
                   nframes = sum(informative))

gif_scatter <- animate(plot_scatter,
                       fps = 2,
                       width = 320,
                       height = 320,
                       nframes = sum(informative))


## Magick trick from Matt Crump https://github.com/thomasp85/gganimate/wiki/Animation-Composition

mgif_lod <- image_read(gif_lod)
mgif_scatter <- image_read(gif_scatter)

new_gif <- image_append(c(mgif_lod[1], mgif_scatter[1]))
for(i in 2:sum(informative)){
  combined <- image_append(c(mgif_lod[i], mgif_scatter[i]))
  new_gif <- c(new_gif, combined)
}

image_write(new_gif, path = "out.gif", format = "gif")
