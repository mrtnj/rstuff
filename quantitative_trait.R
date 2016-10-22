library(magrittr)

N <- 1000

f <- function(p, N) 1 / (2 * log(2 * N) * p * (1 - p))


## rejection
u <- runif(1e6, 0, 1)
y <- rnorm(1e6, 0.5, 0.5)
f_y <- f(y, N)
accept <- ifelse(u < f_y / max(f_y), TRUE, FALSE)


## MH
nit <- 1e5
x <- numeric(nit)
x[1] <- 0.5
s <- 0.1

for (i in 1:(nit - 1)) {
  prop <- rnorm(1, x[i], s)
  a <- f(prop, N)/f(x[i], N)
  if (a > 1)
    x[i + 1] <- prop ## accept
  else {
    if (runif(1, 0, 1) < a)
      x[i + 1] <- prop
    else
      x[i + 1] <- x[i] ## reject
  }
}

samples <- x[(ceiling(nit/2)):nit]

hist(samples, breaks = 100, freq = FALSE)
xs <- seq(0.01, 0.99, by = 0.01)
points(xs, f(xs, N))


genotypes <- data.frame(p = round(sample(samples, 1000), digits = 2)) %>%
  transform(AA = round(N * p^2),
            Aa = round(N * 2 * p * (1 - p))) %>%
  transform(aa = N - AA - Aa)


genotypes <- subset(genotypes, AA != N & aa != N)

## genetic effects
beta <- c(rep(1, round(nrow(genotypes) * 0.1)),
          rep(0, nrow(genotypes) - round(nrow(genotypes) * 0.1)))

## fill in genotypes
X <- matrix(nrow = 1000, ncol = nrow(genotypes))
for (i in 1:ncol(X)) {
  genotype_vector <- c(rep(0, genotypes$AA[i]),
                      rep(1, genotypes$Aa[i]),
                      rep(2, genotypes$aa[i]))
  X[,i] <- sample(genotype_vector)
}

## data
data <- data.frame(id = 1:1000,
                   X,
                   y = as.vector(X %*% beta) + rnorm(1000))

