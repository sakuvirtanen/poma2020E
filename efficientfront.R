filename = "Suomi-osakkeet.xlsx"
return_sheet = "Returns"

# RUN THE BELOW SCRIPT IF readxl LIBRARY IS NOT INSTALLED:
# install.packages("readxl")

###################################################

# Initialize library
library(readxl)

# Read returns:
Returns <- read_excel(filename, sheet = return_sheet)

# Add date colums
Returns$...2 <- format(as.Date(Returns$...2), "%Y-%m")

###################################################
# FILL THE VALUES TO HANDLE STOCKS:
###################################################

Tickers <- c("NOKIA.HE", "FORTUM.HE", "NESTE.HE")   # Tickers as in c("NOKIA.HE", "FORTUM.HE", "NESTE.HE")
Begin = ("2010-10")                                 # Begin month as YYYY-MM e.g. "2010-10"
End = ("2020-10")                                   # End month as YYYY-MM e.g. "2020-10"
Steps = 60                                          # Number of steps to simulate - 1 month = 1 step 
N = 1000                                            # Number of simulatiomn runs
weights_ = matrix(c(1,1,1), nrow = 1)               # Portfolio weights to be scaled as in c(1,1,1). Weights need not to add up to 1. 
Value = 1000000                                     # Portfolio market cap at beginning e.g. 1000000
breaks_ = 15                                        # Number of histrogram breaks

###################################################

# Scales weights to add up to 1:
weights_ = weights_/sum(weights_)

# Find first and last rows to be included:
Begin_index = which(Begin == Returns$...2 )
End_index = which(End == Returns$...2 )

# Select data range of returns to be estimated:
Selected  <- Returns[c(Begin_index: End_index),Tickers]

# covariance and correlation matrices:
Cov_matrix = cov(Selected, use="complete.obs")
Corr_matrix = cor(Selected, use="complete.obs")

# Cholesky decompostion:
Chol_cor = t(chol(Corr_matrix))

# Estimate expected returns:
Exp_returns <- colMeans(Selected, na.rm = T)

# Extract individual stock volatilies from cov matrix:
Volas <- sqrt(diag(Cov_matrix))







# expected portfolio return from dot product of weights and expected returns of individual stocks
portfolio_return <- sum(weights_ * Exp_returns)


# set portfolio vola to 0
portfolio_vola <- 0
# calculate individual stocks' impact to variance
for (i in 1:length(weights_)) {
  portfolio_vola <- portfolio_vola + weights_[i] * weights_[i] * Volas[i] * Volas[i]
}
# calculate impact of covariance between two stocks to variance
for (i in 1:length(weights_)) {
  for (j in 1:length(weights_)) {
    if (i != j) {
      portfolio_vola <- portfolio_vola + 2 * weights_[i] * weights_[j] * Cov_matrix[i,j]
    }
  }
}
#volatility from variance
portfolio_vola <- sqrt(portfolio_vola)

# covariances between everything
frontier_cov <- cov(Returns[c(Begin_index: End_index), names(Returns)[-c(1,2)]])
frontier_cov <- frontier_cov[, colSums(is.na(frontier_cov)) < nrow(frontier_cov)]
frontier_cov <- na.omit(frontier_cov)

# expected returns of everything
frontier_returns <- colMeans(Returns[c(Begin_index: End_index), colnames(frontier_cov)], na.rm = T)

# expected standard deviations of everything
frontier_stdevs <- sqrt(diag(frontier_cov))

ones <- c(rep(1, length(frontier_returns)))

A <- t(ones) %*% solve(frontier_cov) %*% ones
B <- t(ones) %*% solve(frontier_cov) %*% frontier_returns
C <- t(frontier_returns) %*% solve(frontier_cov) %*% frontier_returns
delta <- A * C - B * B

mu <- (1:5000)/70000

Avector <- c(rep(A, length(mu)))
Bvector <- c(rep(B, length(mu)))
Cvector <- c(rep(C, length(mu)))
deltavector <- c(rep(delta, length(mu)))

minvar <- (Avector * mu * mu - 2 * Bvector * mu + Cvector) / deltavector
minstd <- sqrt(minvar)

plot(minstd, mu, main='Portfolio and Efficient Frontier', xlab='Volatility', ylab='Expected return')
points(portfolio_vola, portfolio_return, col = '#51c1e0', pch = 4, cex=3, lwd=3)
