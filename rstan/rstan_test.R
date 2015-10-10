

N <- 10
a <- 0
b <- 0.5
sigma <- 0.5
treatment <- c(rep("control", N), rep("treat", N))
data <- data.frame(id = 1:(2*N),
                   treatment,
                   response = a + as.numeric(treatment == "treat") * b +
                     rnorm(2*N, 0, sigma))

stan_data <- list(n = 2*N,
                  y = data$response,
                  x = as.numeric(data$treatment == "treat"))

model <- stan(model_code = "
     data {
       int <lower = 0> n;
       vector[n] y;
       vector[n] x;
     }
     parameters {
       vector[2] beta;
       real <lower = 0> sigma;
     }
     model {
       beta ~ normal(0, 1);
       y ~ normal(beta[1] + beta[2] * x, sigma);
     }
     ",
     data = stan_data,
     iter = 1000, chains = 4)


model_lm <- lm(response ~ treatment, data)
coef(model_lm)
confint(model_lm)
summary(model_lm)$sigma

print(model)