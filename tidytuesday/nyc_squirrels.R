
## NYC squirrel data 2019-10-28
## https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-10-29


library(lubridate)
library(gganimate)
library(ggplot2)
library(readr)



squirrels <- read_csv("nyc_squirrels.csv")

## Parse the date
squirrels$date_parsed  <- parse_date(as.character(squirrels$date), format = "%m%d%Y")

## Give each observation a unique ID (to use as group in the
## animation, so as to not have points turn into one another but fade
## instead.
squirrels$key  <- 1:nrow(squirrels)


squirrels$image  <- "squirrel.png"

plot_colour <- qplot(y = long, x = lat, colour = primary_fur_color,
                     group = key, geom = "image",
                     data = squirrels) +
    labs(title = "{closest_state}",
         caption = "Squirrel icon made by Freepik from www.flatiron.com.") +
    transition_states(paste(date_parsed, shift),
                      state_length = 2,
                      transition_length = 1) +
    enter_fade() +
    exit_fade()


animate(plot_colour,
        nframes = 50)



## anim_save()
