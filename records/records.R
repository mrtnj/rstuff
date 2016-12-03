
source("records_functions.R")


## Examples of models

no_improvement <- replicate(50, simulate_records(data.frame(intercept = 7, sd = 0.4,
                                                 improvement = 0, improvement_squared = 0))$record)
with_improvement <- replicate(50, simulate_records(data.frame(intercept = 7, sd = 0.4,
                                                              improvement = 0.01, improvement_squared = 0))$record)

melted_no_improvement <- melt(no_improvement)
melted_improvement <- melt(with_improvement)
colnames(melted_no_improvement) <- c("year", "replicate", "record")
colnames(melted_improvement) <- c("year", "replicate", "record")
melted_no_improvement$improvement <- "no improvement"
melted_improvement$improvement <- "improvement"

combined <- rbind(melted_no_improvement, melted_improvement)

simulated_plot <- qplot(x = year, group = replicate, y = record, alpha = I(1/5),
                        data = combined, facets = ~ improvement, geom = "line") + theme_bw()



## Real data

mens_long_jump_records <- read.csv("mens_long_jump_records.txt", comment.char="#")

data_records <- data.frame(year = 1901:2016,
                           meters = NA)
record <- data_records$meters[1] <- mens_long_jump_records$meters[1]
for (i in 2:nrow(data_records)) {
  record <- max(record, subset(mens_long_jump_records, 
                               year == data_records$year[i])$meters)
  data_records$meters[i] <- record
}

data_seasons <- read.csv("mens_long_jump_year_best.txt", comment.char="#")


real_plot <- ggplot() +
  geom_line(aes(x = year, y = meters), data = data_records) +
  geom_point(aes(x = year, y = meters), data = data_seasons)


## ABC models




##posterior_plot1 <- posterior_plot(improvement_model[[2]])
##posterior_plot2 <- posterior_plot(no_improvement_model[[2]])