library(reshape2)
library(ggplot2)

no_improvement <- replicate(50, {
  jumps <- replicate(100, max(rnorm(1000, 7, 0.4)))
  record <- numeric(length(jumps))
  record[1] <- jumps[1]
  for (j in 2:length(jumps)) {
    if (jumps[j] > record[j - 1])
      record[j] <- jumps[j]
    else
      record[j] <- record[j - 1]
  }
  record
})


with_improvement <- replicate(50, {
  jumps <- numeric(100)
  record <- numeric(length(jumps))
  jumps[1] <- max(rnorm(1000, 7, 0.4))
  record[1] <- jumps[1]
  for (j in 2:length(jumps)) {
    jumps[j] <- max(rnorm(1000, 7 + 0.01 * (j - 1), 0.4))
    if (jumps[j] > record[j - 1])
      record[j] <- jumps[j]
    else
      record[j] <- record[j - 1]
  }
  record
})

melted_no_improvement <- melt(no_improvement)
melted_improvement <- melt(with_improvement)
colnames(melted_no_improvement) <- c("year", "replicate", "record")
colnames(melted_improvement) <- c("year", "replicate", "record")
melted_no_improvement$improvement <- "no improvement"
melted_improvement$improvement <- "improvement"

combined <- rbind(melted_no_improvement, melted_improvement)

qplot(x = year, group = replicate, y = record, alpha = I(1/5),
      data = combined, facets = ~ improvement, geom = "line") + theme_bw()
