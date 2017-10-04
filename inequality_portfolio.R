library(Rsolnp)

# Portfolio Returns
ret <- c(health = 1.551, utility = 1.156, other = 1.215)
# Covariances
cov_ <- matrix(c(57.298,    12.221,   33.026,
                12.221,    13.168,   11.814,
                33.026,    11.814,   27.952),
              byrow = TRUE, nrow = 3, ncol = 3,
              dimnames = list(c("health", "utility", "other"),
                              c("health", "utlity", "other")))

support <- seq(-1, 1, by = 0.2)
# Expected return
mu0 <- 1.35
# Portfolio Variance
sigma <- 4.84

# Initial Values of the parameters
init_values <- rep(1/length(support), length(ret)*length(support))

print("Initial values are:")
matrix(init_values, nrow=3, ncol=11)
# Objective function to be minimized 
obj <- function(Pi) {
    h <- ifelse(Pi <= 0, 0, Pi*log(Pi))
    sum(h)
}

# Equality constraints
eqfun <- function(Pi) {
    mat <- matrix(Pi, nrow = 3, ncol = 11)
    ShareC <- sum(mat %*% support)
    proper_probs <- rowSums(mat)
    return(c(ShareC, proper_probs))
}

# Value of equality constrains
eqB <- c(1,1,1,1)

# Inequality contrains
ineqfun <- function(Pi) {
    mat <- matrix(Pi, nrow = 3, ncol = 11)
    share_s <- as.vector(mat %*% support)
    mu0 <- share_s %*% ret
    covar <- share_s %*% cov_ %*% share_s
    return(c(mu0, covar))
}

# Value of inequality constraints
ineqLB <- c(mu0, 0)
ineqUB <- c(Inf, 10*sigma)

# Parameter lower bounds (pi must be positive)
LB <- rep(0, length(init_values))

solution <- solnp(pars = init_values, 
                  fun = obj,
                  eqfun = eqfun, 
                  eqB = eqB, 
                  ineqfun = ineqfun, 
                  ineqLB = ineqLB, 
                  ineqUB = ineqUB, 
                  LB = LB)

matrix(solution$pars, nrow = 3, ncol = 11)
matrix(solution$pars, nrow = 3, ncol = 11) %*% support
