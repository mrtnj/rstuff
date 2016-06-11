
## Plotting script for blogpost on balancing a centrifuge.

library(magrittr)
library(plyr)
library(ggplot2)
library(reshape2)

theta <- function(n, N) (n - 1) * 2 * pi / N
tube <- function(theta) c(x = cos(theta), y = sin(theta))

positions <- 1:30
coordinates <- data.frame(position = positions,
                          positions %>% lapply(theta, N = 30) %>% ldply(tube))

coordinates$configuration1 <- coordinates$configuration2 <- coordinates$configuration3 <- "empty"
coordinates$configuration1[c(1, 11, 14, 15, 21, 29, 30)] <- "used"
coordinates$configuration3[c(4, 8, 14, 15, 21, 27, 28)] <- "used"
coordinates$configuration2 <- ifelse(coordinates$configuration1 == "used", "empty", "used")

melted <- melt(coordinates, id.vars = c("x", "y", "position"))
melted$variable <- factor(melted$variable,
                          levels = c("configuration1", "configuration2", "configuration3"))

configuration_plot <- qplot(x = x, y = y, label = position,
                            geom = "text", colour = value,
                            data = melted) +
  facet_wrap(~ variable) +
  theme_bw() 

