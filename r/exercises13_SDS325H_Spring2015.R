N = 100

# Simulate the nodes
B = rnorm(N, 0, 3)
X = B + rnorm(N, 0, 1)
C = B + rnorm(N, 0, 1)
M = X + B + rnorm(N, 0, 1)
Y = M + C + rnorm(N, 0, 1)

# So the true causal effect is dY/dX = dY/dM * dM/dX = 1 * 1 = 1

# Naive
lm1 = lm(Y~X)
summary(lm1)

# Kitchen sink
lm2 = lm(Y ~ X + M + B + C)
summary(lm2)

# Front-door adjustment
lm3a = lm(M ~ X + B)
summary(lm3a)

lm3b = lm(Y ~ M + C)
summary(lm3b)

# Combine to get causal estimate
beta_hat = coef(lm3a)[2] * coef(lm3b)[2]
beta_hat
