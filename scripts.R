###################################################
# FILL THE VALUES BELOW TO CORRESPOND WORKSPACE
###################################################

stocks_filename = "data/Suomi-osakkeet.xlsx"
return_sheet = "Returns"

bonds_filename = "data/finnish_corporate_bonds.xlsx"
bond_sheet = "Sheet1"

source("Bond_price.r")
source("Bond_price_vector.r")
source("Bond_price_matrix.r")
source("Bond_sim.r")


# RUN THE BELOW SCRIPT IF readxl OR lubridate LIBRARIES ARE NOT INSTALLED:
# install.packages("readxl")
# install.packages("lubridate")

###################################################

# Initialize library
library(readxl)
library(lubridate) 
library(zoo)

# Read returns:
Returns <- read_excel(stocks_filename, sheet = return_sheet)

# Add date colums
Returns$...2 <- format(as.Date(Returns$...2), "%Y-%m")

# Read bond data:
Bonds <- read_excel(bonds_filename, sheet = bond_sheet)

###################################################
# FILL THE ASSETS CLASS ALLOCATIONS:
###################################################
# Allocation between bonds and stock:
# bond_alloc = 1
# stock_alloc = 1
# bond_stock = c(bond_alloc,stock_alloc)
# Scale to sum 1:
# bond_stock = bond_stock/sum(bond_stock)
###################################################

###################################################
# FILL THE VALUES TO HANDLE STOCKS:
###################################################

# Tickers <- c("NOKIA.HE", "FORTUM.HE", "NESTE.HE", "ASPO.HE")   # Tickers as in c("NOKIA.HE", "FORTUM.HE", "NESTE.HE")
# Begin = ("2018-10")                                 # Begin month as YYYY-MM e.g. "2010-10"
# End = ("2020-10")                                   # End month as YYYY-MM e.g. "2020-10"
# Steps = 24                                          # Number of steps to simulate - 1 month = 1 step 
# N = 100                                             # Number of simulatiomn runs
# weights_stock = matrix(c(1,1,1,1), nrow = 1)        # Portfolio stock weights to be scaled as in c(1,1,1). Weights need not to add up to 1. 
# Value = 1000000                                     # Portfolio market cap at beginning e.g. 1000000
# breaks_ = 15                                        # Number of histrogram breaks

###################################################
# FILL THE VALUES TO HANDLE THE BONDS:
###################################################

#ISIN = c("XS1333685409", "JP524605AFB4")            # ISINs as in c("XS1333685409", "JP524605AFB4")
#weights_bond = matrix(c(1,1), nrow = 1)             # Portfolio bond weights to be scaled as in c(1,1). Weights need not to add up to 1. 

###################################################
# VALUE AT RISK:
VaR_ = 0.05
###################################################

# If tickers are set: run stock simulation:
Simulate_Stocks <- function(Tickers, Begin, End, Steps, N, weights_stock, Value, stock_alloc, bond_alloc) { 
  
  # Scale bond & stock weights
  bond_stock = c(bond_alloc,stock_alloc)
  bond_stock = bond_stock/sum(bond_stock)
  
  if (length(Tickers > 0)) {
    
    Results <- matrix(ncol = Steps + 1, nrow = N)
    
    # Scales weights to add up to 1:
    weights_stock = bond_stock[2]*weights_stock/sum(weights_stock)
    
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
    
    # Initialize matrix for simulation results: N rows and Steps + 1 columns
    # Results = matrix(ncol = Steps + 1, nrow = N)
    
    # Loop each simulation
    for (i in 1:N) {
      # Create a matrix for one simulation run:
      Start = matrix(nrow = ncol(Selected), ncol = Steps + 1)
      # At the beginning, we use normalization scaling each stock to one:
      Start[,1] = weights_stock/weights_stock
      
      # Generate random movement standard normal variables for the stocks and steps:
      Randoms <- matrix(rnorm(ncol(Selected)*Steps, mean = 0, sd = 1), ncol(Selected), Steps) 
      # Multiply generated standard normal random variablesa with Cholesky matrix:
      Corr_Randoms <- Chol_cor%*%Randoms
      
      # Loop each step:
      for (j in 1:Steps) {
        # Loop each stock:
        for (k in 1:ncol(Selected)) {
          # Generate next random step using the correlated volatilities and GBM:
          Start[k,j+1] <- Start[k,j]*exp(Exp_returns[k]*1 + Corr_Randoms[k,j]*Volas[k]*sqrt(1))
        }
      }
      # Normalize portfolio values on this simulation run:
      Worths <- weights_stock%*%Start
      # Add simulation run to results:
      # print(Results[i,])
      # print(Worths)
      Results[i,] <- Worths
    }
  # Otherwise fill portfolio with stock weight of cash:
  } else {
    Results = bond_stock[2]*matrix(1, ncol = Steps + 1, nrow = N)
  }
  
  return(Results)
}

Simulate_Bonds <- function(ISIN,Begin,End,Steps,N,bondWeights,stockOnly,stock_alloc,bond_alloc) {
  
  bond_stock = c(bond_alloc,stock_alloc)
  bond_stock = bond_stock/sum(bond_stock)
  
  if (length(ISIN) > 0) {
    # Loop each bond:
    for (i in length(ISIN)) {
      ISIN_ = ISIN[i]
      # Set total portfolio weight:
      w_ = bond_stock[1]*bondWeights[i]
      # Run bond simulation:
      simResult = stockOnly + w_*Bond_sim(ISIN_, N, Begin, End, Steps)
    }
    # If no ISIN given - fill bond share with cash:
  } else {
    w_ = bond_stock[1]
    simResult = stockOnly + w_*matrix(1, nrow = nrow(stockOnly), ncol = ncol(stockOnly))
  }
  
  
  return(simResult)
}


# 
# # Normalized values at end of period:
# Scaled = Results[,Steps+1]
# # Normalized returns at end of period:
# Scaled_return = Results[,Steps+1]/Results[,1]*100-100
# # Calculate market caps at the end of period:
# Cap = Scaled*Value
# 
# # Set up histograms:
# # Title and subtitle:
# if (length(Tickers)>0 && length(ISIN)>0) {
#   title_ret = paste(c("Return simulation with: \n"), paste(Tickers, collapse = ', '), "\n", paste(ISIN, collapse = ', '))
#   title_cap = paste(c("Market cap simulation with: \n"), paste(Tickers, collapse = ', '), "\n", paste(ISIN, collapse = ', '))
# } else if (length(Tickers)>0 && length(ISIN)<1) {
#   title_ret = paste(c("Return simulation with: \n"), paste(Tickers, collapse = ', '))
#   title_cap = paste(c("Market cap simulation with: \n"), paste(Tickers, collapse = ', '))
# } else {
#   title_ret = paste(c("Return simulation with: \n"), paste(ISIN, collapse = ', '))
#   title_cap = paste(c("Market cap simulation with: \n"), paste(ISIN, collapse = ', '))
# }
# 
# VaR_q = quantile(Scaled_return, probs = c(VaR_))*Value/100
# subtitle = paste(N, " simulations, ", Steps, " steps" , ", VaR ", VaR_,"%:" , signif(VaR_q, digits = 3))
# 
# # Generate histograms:
# par(mar = (5*c(1,1,1,1)))
# hist(Scaled_return, main = title_ret, sub = subtitle, xlab = "Cumulative return (%)", xlim = c(min(Scaled_return), max(Scaled_return)), breaks = breaks_)
# hist(Cap, main = title_cap, sub = subtitle, xlab = "Market value (Eur)", xlim = c(min(Cap), max(Cap)), breaks = breaks_)
