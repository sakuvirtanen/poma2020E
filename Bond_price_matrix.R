# A function to calculate bond price on the set date "Date"
# Inputs: 
# Begin_date - First date of simulation
# Date - Date on which price is calculated
# Maturity - Maturity date
# Yield - Yield to maturity
# Freq - Coupon frequency. Values: "Annual", "Semi", "Qrtrly", NA - no coupon.

Bond_price_matrix <- function(Begin_Date, Date, Maturity, Steps, Yield, Freq, Coupon) {
  # Initialize present value matrix:
  PV = matrix(0, nrow = nrow(Yield), ncol = Steps + 1)
  # Initialize discount frequency as number:
  F_ = 0
  
  # Initialize coupon variable:
  Coupon = Coupon
  # Set coupon to zero and discount frequency to 1 per year if Coupon is NA i.e. no coupon:
  if (is.na(Freq)) {
    Coupon = 0
    F_ = 1
  }
  # Annual coupon: 1 discount per year
  else if (Freq == "Annual") {
    F_ = 1
  }
  # Semi-annual coupon: 2 discounts per year
  else if (Freq == "Semi") {
    F_ = 2
    # Quarterly coupon: 4 discounts per year
  } else if (Freq == "Qrtrly") {
    F_ = 4
    # Otherwise assume 1 discount round per year:
  } else {
    F_ = 1
  }
  
  # Loop steps:
  for (j in 1:(Steps+1)) {
    Date = as.Date(paste0(as.character(Begin_Date), "-", 01), format="%Y-%m-%d") %m+% months(j-1)
    # Loop through yields:
    for (i in 1:nrow(Yield)) {
      # Present value of principal and last coupon:
      PV[i,j] = PV[i,j] + (Coupon/F_ + 1)/(1+Yield[i,j]/F_)^(max((interval(Date, Maturity) %/% months(1))/(12/F_),0))
      # Set rolling date to discount coupons to the coupon date preceding maturity:
      Wip_date = Maturity %m-% months(12/F_)
      # Limit while loop steps:
      Step_limit = 0
      # Loop coupons until beginning date of simulation is reached.
      # Note: If coupon is paid before current date, the coupon is assumed to be
      # as cash in the portfolio:
      while (interval(Begin_Date, Wip_date) > 0 && Step_limit < 100) {
        # Calculate PV of coupon on copon date Wip_date. 
        # If simulation Date "Date" has passed coupon Date, the discount is zero:
        PV[i,j] = PV[i,j] + (Coupon/F_)/(1+Yield[i,j]/F_)^(max((interval(Date, Wip_date) %/% months(1))/(12/F_),0))
        # Set the previous coupon date:
        Wip_date = Wip_date %m-% months(12/F_)
        # Increase Step count
        Step_limit =+ 1
      }
    }
  }
  return(PV)
}