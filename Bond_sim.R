
Bond_sim <- function(ISIN, N, Begin_Date, End_Date, Steps) {
  
  # Date conversion for matching data ranges:
  Begin_Date_ = format(Begin_Date, "%Y-%m")
  End_Date_ = format(End_Date, "%Y-%m")
  Sim_First_Date = End_Date_
  
  # Extract bond data:
  Bond_history = Bonds[Bonds$ISIN==ISIN,]
  # Format dates to make matching
  Bond_history$Date = format(as.Date(Bond_history$Date), "%Y-%m")
  
  # Store maturity day:
  Maturity = as.Date(Bond_history$Maturity[1])
  # Store coupon frequency:
  Coupon_frequency = Bond_history$`Coupon Freq`[1]
  # Store coupon amount:
  Coupon = Bond_history$Coupon[1]
  # Store yield history:
  Yields = Bond_history$`Mid Yield`[c(which(Bond_history$Date == Begin_Date_):which(Bond_history$Date == End_Date_))]
  
  # Vasicek model calibration:
  
  # Estimate the regression:
  Yield_reg = lm(Yields[c(2:length(Yields))] ~ Yields[c(1:length(Yields)-1)])
  # Estimate lambda i.e. reversion speed:
  lambda = as.numeric(1 - Yield_reg$coefficients[2])*12
  # Estimate myy i.e. mean yield:
  myy = as.numeric(Yield_reg$coefficients[1]/(1-Yield_reg$coefficients[2]))
  # Estimate sigma i.e. volatility:
  sigma = sqrt(var(Yield_reg$residuals)*12)
  
  # Initialize random variables:
  randoms = matrix(rnorm(Steps*N, mean = 0, sd = 1), nrow = N, ncol = Steps) 
  # Initialize matrix for simualted yields:
  yields_sim = matrix(0, nrow = N, ncol = Steps + 1) 
  
  # Loop N simulation runs:
  for (i in 1:N) {
    # Choose normal random variables for yield simulation:
    randoms_ = randoms[i,]
    
    # Set initial yield from beginning of simulation:
    yields_sim[i,1] = Bond_history$`Mid Yield`[which(Bond_history$Date == Sim_First_Date_)]
    
    # Loop through simulated yields:
    for (j in 2:ncol(yields_sim)) {
      # Set yield:
      yields_sim[i,j] = yields_sim[i,j-1] + (myy-yields_sim[i,j-1])*1/12 + sigma*randoms_[j-1]*sqrt(1/12)
    }
  }
  
  # Initialize matrix for simulated prices at the end:
  Simulations = Bond_price_matrix(Begin_Date, Sim_First_Date, Maturity, Steps, yields_sim/100, Coupon_frequency, Coupon/100)
  # return results:
  return(Simulations)
}