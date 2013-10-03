## styrka
power.t.test(n=30, sd=7, sig.level=0.05, delta=2)
power.t.test(n=30, sd=7, sig.level=0.05, delta=2.5)
power.t.test(n=30, sd=7, sig.level=0.05, delta=3)

## simulering
treatment <- c(rep(0, 30), rep(1, 30))
sim.data <- function(effect, n) {
  replicate(n, 0 + treatment*effect + rnorm(60, 0, 7))  
}

## modell
make.model <- function(sim.data) {
  alply(sim.data, 2, function(x) {lm(x ~ treatment)})
}

## skattningar
summarise.model <- function(x) {
  ests <- summary(x)
  data.frame(p=ests$coefficients["treatment","Pr(>|t|)"],
             estimate=ests$coefficients["treatment", "Estimate"])
}

## designanalys
design.analysis <- function(x, effect) {
  significant <- subset(x, p < 0.05)
  sign.error <- subset(significant, sign(estimate) != sign(effect))
  data.frame(power=nrow(significant)/nrow(x),
             sign.error=nrow(sign.error)/nrow(significant),
             exaggeration=mean(abs(significant$estimate))/effect)
}


## applicera funktionerna i tur och ordning

library(plyr)

effects <- c(1, 1.5, 2, 2.5, 3, 4, 5)

result <- ldply(effects, function(x) {
  data <- sim.data(x, 1000)
  models <- make.model(data)
  summaries <- ldply(models, summarise.model)
  data.frame(effect.size=x,
             design.analysis(summaries, x))
})

result

## rita resultat

library(reshape2)
library(ggplot2)

plot.theme <- theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

## diagram av felrisker

p1 <- qplot(x=effect.size, y=value, colour=variable,
            geom="line", data=melt(result[,1:3],
                                   id.vars="effect.size"),
            ylim=c(0,1), xlab="Verklig effekt",
            ylab="Sannolikhet") +
  geom_vline(x=2.5, colour="grey") +
  plot.theme

## diagram av exaggeration factor

p2 <- qplot(x=effect.size, y=exaggeration, geom="line",
            data=result, ylim=c(0, 5),
            xlab="Verklig effekt", ylab="Överdriftsfaktor") + 
  geom_vline(x=2.5, colour="grey") + plot.theme
