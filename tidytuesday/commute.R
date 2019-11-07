
## Walking and biking data 2019-11-05
## https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-11-05


library(dplyr)
library(ggplot2)
library(maps)
library(readr)
library(usmap)


commute <- read_csv("commute.csv")


## Map data from the usmap package

state_map  <- us_map(regions = "state")


## There are some incompletely labelled states; fix them

missing  <- setdiff(commute$state, state_map$full)

commute$state_modified <- commute$state
commute$state_modified[commute$state == "Ca"] <- "California"
commute$state_modified[commute$state == "Massachusett"]  <- "Massachusetts"


## Get the average per state

state_average  <- summarise(group_by(commute, state_modified, mode),
                            average = mean(percent))

## Combine averages and coordinates

combined  <- inner_join(state_average,
                        state_map,
                        by = c("state_modified" = "full"))

## Draw!

plot_map  <- ggplot() +
    geom_polygon(aes(x = x, y = y, fill = average, group = group),
                 colour = "black",
                 data = combined) +
    facet_wrap(~ mode) +
    scale_fill_continuous(low = "white",
                          high = "blue",
                          name = "Percent commuters") +
    theme_bw(base_size = 16) +
    theme(panel.grid = element_blank(),
          strip.background = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom") +
    xlab("") +
    ylab("") +
    labs(caption = "Cycling and walking to work 2008-2012 in the American Community Survey.")


pdf("commute_map.pdf", width = 14)
print(plot_map)
dev.off()
