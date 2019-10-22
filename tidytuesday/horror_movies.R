
## Horror movie data 2019-10-22
## https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-10-22

library(dplyr)
library(egg)
library(ggplot2)
library(ggimage)
library(lubridate)
library(readr)
library(stringr)


movies <- read_csv("horror_movies.csv")


## Parse dates

movies$release_parsed  <- parse_date(movies$release_date,
                                     format = "%d-%b-%y",
                                     locale = locale("en")) 


movies$release_year <- ifelse(is.na(movies$release_parsed),
                              movies$release_date,
                              year(movies$release_parsed))

movies$release_month  <- month.abb[month(movies$release_parsed)]


## Parse budget

movies$budget_currency  <- str_match(movies$budget,
                                     "^[^0-9]+")[,1]

movies$budget_numeric  <- as.numeric(gsub(movies$budget, pattern = "[^0-9]",
                                          replacement = ""))

## Count films per year

count <- as.data.frame(table(movies$release_month))

colnames(count) <- c("release_month", "number")


## Linear model of average rating

model  <- lm(review_rating ~ release_month, movies)

fit  <- data.frame(release_month = month.abb,
                   predict(model,
                           newdata = data.frame(release_month = month.abb),
                           interval = "confidence"),
                   stringsAsFactors = FALSE)

grand_mean_rating  <- mean(movies$review_rating,
                           na.rm = TRUE)

## Linear model of log budget

model_budget  <- lm(log10(budget_numeric) ~ release_month, movies)

fit_budget  <- data.frame(release_month = month.abb,
                          predict(model_budget,
                                  newdata = data.frame(release_month = month.abb),
                                  interval = "confidence"),
                          stringsAsFactors = FALSE)



## Plot of month

plot_number <- ggplot() +
    geom_bar(aes(x = release_month,
                   y = number),
             data = count,
             stat = "identity",
             fill = "grey") +
    scale_x_discrete(limits = month.abb) +
    theme_bw(base_size = 12) +
    theme(panel.grid = element_blank()) +
    xlab("") +
    ylab("Number of films released")
    

plot_rating <- ggplot() +
    geom_violin(aes(x = release_month,
                    y = review_rating),
                fill = "grey",
                colour = NA,
                data = movies) +
    scale_x_discrete(limits = month.abb) +
    geom_pointrange(aes(x = release_month,
                        y = fit,
                        ymax = upr,
                        ymin = lwr),
                    data = fit) +
    geom_hline(yintercept = grand_mean_rating,
               linetype = 2,
               colour = "red") +
    ylim(0, 10) +
    theme_bw(base_size = 12) +
    theme(panel.grid = element_blank()) +
    xlab("") +
    ylab("Review rating")


plot_budget <- ggplot() +
    geom_violin(aes(x = release_month,
                    y = log10(budget_numeric)),
                data = filter(movies,
                              budget_currency == "$"),
                fill = "grey",
                colour = NA) +
    geom_pointrange(aes(x = release_month,
                        y = fit,
                        ymax = upr,
                        ymin = lwr),
                    data = fit_budget) +
    scale_x_discrete(limits = month.abb) +
    scale_y_continuous(breaks = c(4, 6, 8),
                       labels = c(0.01, 1, 100)) +
    theme_bw(base_size = 12) +
    theme(panel.grid = element_blank()) +
    xlab("Release month") +
    ylab("Million dollars budget")


plot_combined <- ggarrange(plot_number,
                           plot_rating,
                           plot_budget,
                           top = "September and October are the horror film months")


pdf("movies.pdf")
print(plot_combined)
dev.off()




## Plot of day

movies$yday  <- yday(movies$release_parsed)

daycount <- summarise(group_by(movies, yday, release_year), n = n())

halloween  <-  yday("2019-10-31")

pumpkin_data  <- data.frame(x = halloween,
                            y = -1,
                            image = "pumpkin.png", stringsAsFactors = FALSE)

breaks  <- yday(paste("2019-", 1:12, "-01", sep = ""))


plot_year <- ggplot() +
    geom_point(aes(x = yday,
                   y = n),
               colour = "green",
               data = na.exclude(dc)) +
    geom_image(aes(x = x,
                   y = y,
                   image = image),
               data = pumpkin_data) +
    facet_wrap(~ release_year,
               ncol = 2) +
    scale_x_continuous(breaks = breaks,
                       labels = month.abb) +
    ylim(-3, NA) +
    labs(caption = "Pumpkin icon by Good Ware from www.flatiron.com.") +
    theme(panel.grid = element_blank(),
          strip.background = element_blank(),
          text = element_text(family = "mono",
                              colour = "grey",
                              size = 16),
          axis.text = element_text(family = "mono",
                                   colour = "green",
                                   size = 14),
          axis.ticks = element_line(colour = "green"),
          strip.text = element_text(family = "mono",
                                    colour = "grey",
                                    size = 16),
          plot.background = element_rect(fill = "black"),
          panel.background = element_rect(fill = "black")) +
    xlab("") +
    ylab("Horror films released on this day") +
    ggtitle("When horror films are released")




pdf("movies_year.pdf", width = 14)
print(plot_year)
dev.off()
