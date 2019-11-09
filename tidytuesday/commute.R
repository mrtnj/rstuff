
## Walking and biking data 2019-11-05
## https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-11-05


library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(swemaps)
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
                            average = sum(percent * n)/sum(n))

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



## Sweden

commute_sweden <- read_csv("commute_sweden.csv",
                           comment = "#")
colnames(commute_sweden)[2:3]  <- c("walking", "bike") 

## Duplicate Kalmar--Gotland row
ix  <- which(commute_sweden$namn == "Kalmar län;Gotlands län")

extra_rows  <- commute_sweden[c(ix, ix),]
extra_rows$namn  <- unlist(strsplit(extra_rows$namn[1],
                                    split =  ";"))

commute_sweden_modified  <- rbind(commute_sweden[-ix,],
                                  extra_rows)

## Separate the two variables
commute_sweden_long  <-  pivot_longer(commute_sweden_modified,
                                      c("walking", "bike"),
                                      names_to = "variable",
                                      values_to = "value")

combined_sweden <- full_join(commute_sweden_long,
                             map_ln,
                             by = c("namn" = "lnnamn"))


plot_sweden <- ggplot() +
    geom_polygon(aes(x = ggplot_long,
                     y = ggplot_lat,
                     group = namn,
                     fill = value),
                 colour = "black",
                 data = combined_sweden) +
    coord_equal() +
    facet_wrap(~ variable) +
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
    labs(caption = "Swedish commuters by län 2005-2006. Data from VTI.")


pdf("commute_sweden_map.pdf")
print(plot_sweden)
dev.off()
