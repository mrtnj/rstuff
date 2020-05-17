
## Quick reminder on how to set colour scales

library(ggplot2)
library(tidyr)
library(readr)


## Make up some data

x <- 1:100

beta <- rnorm(5, 1, 0.5)

stroop <- data.frame(x,
                     sapply(beta, function(b) x * b + rnorm(100, 1, 10)))
colnames(stroop)[2:6] <- c("orange", "blue", "red", "purple", "green") 

data_long <- pivot_longer(stroop, -x)

plot_y <- qplot(x = x,
                y = value,
                colour = name,
                data = data_long) +
  theme_minimal() +
  theme(panel.grid = element_blank())



## Read colours

colours <- read_csv("scale_colours.csv")

plot_y_colours <- plot_y + 
  scale_colour_manual(limits = colours$name,
                      values = colours$colour)

