some.data <- rnorm(10, 4, 1.5)
names(some.data) <- 1:10
barplot(some.data)


library(ggplot2)
library(reshape2)
library(plyr)


## Simulate some data

n <- 10
group <- rep(1:4, n)
mass.means <- c(10, 20, 15, 30)
mass.sigma <- 4
score.means <- c(5, 5, 7, 4)
score.sigma <- 3
mass <- as.vector(model.matrix(~0+factor(group)) %*% mass.means) +
  rnorm(n*4, 0, mass.sigma)
score <- as.vector(model.matrix(~0+factor(group)) %*% score.means) +
  rnorm(n*4, 0, score.sigma)

data <- data.frame(id = 1:(n*4), group, mass, score)


## Calculate means

melted <- melt(data, id.vars=c("id", "group"))

means <- ddply(melted, c("group", "variable"), summarise,
               mean=mean(value))


## Plot means
means.barplot <- qplot(x=group, y=mean, fill=variable,
                       data=means, geom="bar", stat="identity",
                       position="dodge")


## Add standard error of the mean

means.sem <- ddply(melted, c("group", "variable"), summarise,
                   mean=mean(value), sem=sd(value)/sqrt(length(value)))
means.sem <- transform(means.sem, lower=mean-sem, upper=mean+sem)

means.barplot + geom_errorbar(aes(ymax=upper,
                                  ymin=lower),
                              position=position_dodge(0.9),
                              data=means.sem)

