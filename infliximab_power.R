## ursprunglig styrka
power.t.test(n=30, sd=7, sig.level=0.05, power=0.9)
power.t.test(n=30, sd=7, sig.level=0.05, power=0.5)

## styrka med skillnad 3
power.t.test(delta=3.1, sd=7, sig.level=0.05, power=0.8)


## simulering
power.t.test(n=11, sd=7, sig.level=0.05, delta=5)

treatment22 <- c(rep(0, 11), rep(1, 11))
sim.data22 <- replicate(1000, 0 + treatment22*5 + rnorm(22, 0, 7))
treatment60 <- c(rep(0, 30), rep(1, 30))
sim.data60 <- replicate(1000, 0 + treatment60*5 + rnorm(60, 0, 7))
treatment100 <- c(rep(0, 50), rep(1, 50))
sim.data100 <- replicate(1000, 0 + treatment100*5 + rnorm(100, 0, 7))


library(plyr)
## skattningar
est.delta <- function(sim.data, treatment) {
  est <- unlist(alply(sim.data, 2, function(x) { 
    model <- lm(x ~ treatment)
    p <- summary(model)$coefficients[2,4]
    if (p < 0.05) {
      return(coef(model)[2])
    }
  }))
  est
}

est22 <- est.delta(sim.data22, treatment22)
length(est22)
est60 <- est.delta(sim.data60, treatment60)
length(est60)
est100 <- est.delta(sim.data100, treatment100)
length(est100)

## histogram

library(ggplot2)
n22 <- qplot(x=est22, xlab="Skillnad mellan grupperna",
             ylab="Antal",
             main="22 deltagare", xlim=c(0,15))
n60 <- qplot(x=est60, xlab="Skillnad mellan grupperna",
             ylab="Antal",
             main="50 deltagare", xlim=c(0,15))
n100 <- qplot(x=est100, xlab="Skillnad mellan grupperna",
             ylab="Antal",
             main="100 deltagare", xlim=c(0,15))


