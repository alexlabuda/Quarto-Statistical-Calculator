# Calculating MDE

# 1 Treatment, 1 Control

# MDE (n) = 2 * (Zα + Zβ)^2 * (σ^2) / (μ1 - μ2)^2

# success rate = 0.05
# alpha        = 0.05
# beta         = 0.2
# power        = 0.8
# t(1-k)       = 1.282
# t-stat       = 1.96

# MDE(N) = ((t-stat of alpha) + t(1-k)) * sqrt(1/(p*(1-P))) * sqrt(sigma^2/N)

# Solve when our total sample size is 1000
0.05 * (1 - 0.05)
pooled_variance <- 0.05*(1-0.05) + 0.05*(1-0.05)

((1.96 + 1.282) * sqrt(1/(.5*(1-.5))) * sqrt(pooled_variance/1000))
