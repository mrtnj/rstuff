
## NYC squirrel data 2019-10-28
## https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-10-29


library(dplyr)
library(gganimate)
library(ggimage)
library(ggplot2)
library(readr)



squirrels <- read_csv("nyc_squirrels.csv")

## Parse the date
squirrels$date_parsed  <- parse_date(as.character(squirrels$date), format = "%m%d%Y")

## Give each observation a unique ID (to use as group in the
## animation, so as to not have points turn into one another but fade
## instead.
squirrels$key  <- 1:nrow(squirrels)


## Associate the different squirrel colours with the filenames of
## icons in different colours (manually filled with GIMP).
squirrels$image  <- "squirrel.png"
squirrels$image[squirrels$primary_fur_color == "Cinnamon"]  <- "squirrel_cinnamon.png"
squirrels$image[squirrels$primary_fur_color == "Gray"]  <- "squirrel_grey.png"
squirrels$image[is.na(squirrels$primary_fur_colour)]  <- NA


## Rendering takes time. This is a still image on a subsampled dataset for testing.
ix  <- sample(1:nrow(squirrels), 100)

plot_still  <- ggplot() +
    geom_image(aes(y = long, x = lat, image = image, group = key),
               size = 0.06,
               data = filter(squirrels, age == "Adult")[ix,]) +
    geom_image(aes(y = long, x = lat, image = image, group = key),
               size = 0.04,
               data = filter(squirrels, age == "Juvenile")[ix,]) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    xlab("Latitude") +
    ylab("Longitude")


## The actual animation
plot_colour <- ggplot() +
    geom_image(aes(y = long, x = lat, image = image, group = key),
               size = 0.05,
               data = filter(squirrels, age == "Adult")) +
    geom_image(aes(y = long, x = lat, image = image, group = key),
               size = 0.03,
               data = filter(squirrels, age == "Juvenile")) +
    theme_bw(base_size = 16) +
    theme(panel.grid = element_blank()) +
    xlab("Latitude") +
    ylab("Longitude") +
    labs(title = "{closest_state}",
         caption = "Data from NYC Squirrel Census. Squirrel icon made by Freepik from www.flatiron.com.") +
    transition_states(paste(date_parsed, shift),
                      state_length = 2,
                      transition_length = 1)


## Render it and write to file
animate(plot_colour,
        fps = 10,
        nframes = 400,
        end_pause = 20,
        rewind = FALSE,
        width = 1000,
        height = 1000)


anim_save("nyc_squirrels.gif")
