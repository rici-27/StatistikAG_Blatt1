#This is the code for ex sheet 1

setwd("~/Desktop/Statistik_AG/Blatt1")

#Project risk measures: financial risk

###########################

#task 1:
#--Illustrate the time series of log-returns for the assets over time. 
#--i.e. for prices (Pt)1⩽t⩽T +1 consider the time series (log(Pt+1) − log(Pt))1⩽t⩽T 
#--It is standard to compute returns based on daily close values. 
#--The x-axis should show the date, the y-axis the value of the corresponding log-return.

#We only use Microsoft-data.

#Load the csv to R:
data <- read.csv("MicrosoftData.csv")
head(data)

#only the close values:
head(data$Close.Last)

#shift them one up:
data$shifted_close_value <- c(data$Close.Last[-1], NA)

#shifted dates:
data$shifted_date <- c(data$Date[-1], NA)

#prices as numeric: 
#$ symbol cut away and save as numeric

#data$Pt <- as.numeric(data$Close.Last)
#data$Ptm1 <-as.numeric(data$shifted_close_value)
data$Pt <- as.numeric(substr(data$Close.Last, 2, nchar(data$Close.Last)))
data$Ptm1 <-as.numeric(substr(data$shifted_close_value, 2, nchar(data$shifted_close_value)))

#log returns
data$log_returns <- log(data$Pt)-log(data$Ptm1)
View(data)

#Control: data
head(data)


#Now we plot:

dates_plot<-as.Date(rev(data$shifted_date[-nrow(data)]), format = "%m/%d/%Y")
log_returns_plot <-rev(data$log_returns[-nrow(data)])

plot(dates_plot, log_returns_plot, type = "p", col = "black", 
     xlab = "Date", ylab = "log returns", 
     main = "Prices Over 10 Years", xaxt = "n") 

# Define the specific dates for the x-axis
x_axis_dates <- c(seq(as.Date("2014-10-29"), as.Date("2024-10-25"), by = "12 month"), as.Date("2024-10-25"))

# Add the custom x-axis
axis(1, at = x_axis_dates, labels = format(x_axis_dates, "%m/%d/%Y"), las = 1)


###########################

#task 2:
#--Use acf to illustrate autocorrelations of log-returns and squared log-returns. Interpret the outcomes.

#log returns
acf(log_returns_plot, main = "ACF of log returns")

#the values
acf_result_log_returns <- acf(log_returns_plot, plot = FALSE)
acf_result_log_returns


#squared log returns
sq_log_returns_plot <- log_returns_plot*log_returns_plot
acf(sq_log_returns_plot, main = "ACF of log returns")

#the values
acf_result_sq_log_returns <- acf(sq_log_returns_plot, plot = FALSE)
acf_result_sq_log_returns


###########################

#task 3:
#--For α = 0.95, determine the α-VaR and ES to quantify the risk associated with the assets using the RiskMetrics model. 
#--Recall that for RiskMetrics we suppose σ^2_t = λσ^2_(t−1) + (1 − λ)R^2_t  , t = 1, . . . , T
#--for the (squared) volatility σ^2_t, and log-returns R_t.
#--Choose λ = 0.94 and initialize by setting σ^2_1 = R^2_1.
#--The conditional VaR and ES at the end of the observation interval are obtained with the variance σ_T based on a normal hypothesis RT +1 ∼ N (0, σ^2_T). 
#--Illustrate the volatility time series.

#parameters
a<-0.95
l<-0.94

#new df
d3<-data.frame(dates = dates_plot, log_returns = log_returns_plot, sqr_log_returns=sq_log_returns_plot)
d3$sigma_sq <- NA

# Define the first value of C
d3$sigma_sq[1] <- d3$sqr_log_returns[1]

# Loop to fill in the rest of the column
for (i in 2:nrow(d3)) {
  d3$sigma_sq[i] <- (1-l)*d3$sqr_log_returns[i] + l*d3$sigma_sq[i - 1]
}

# View the updated data frame
head(d3)

#conditional VAR at T+1 for the log return
cond_a_Var <- qnorm(a)*sqrt(d3$sigma_sq[nrow(d3)])

#conditional ES at T+1 for the log return
cond_a_ES <- dnorm(qnorm(a))*sqrt(d3$sigma_sq[nrow(d3)])/(1-a)

print(cond_a_Var)
print(cond_a_ES)

#Illustrate the volatility time series.

plot(d3$dates, sqrt(d3$sigma_sq), type = "p", col = "black", 
     xlab = "Date", ylab = "sigma", 
     main = "Vola Over 10 Years", xaxt = "n") 

# Define the specific dates for the x-axis
x_axis_dates <- c(seq(as.Date("2014-10-29"), as.Date("2024-10-25"), by = "12 month"), as.Date("2024-10-25"))

# Add the custom x-axis
axis(1, at = x_axis_dates, labels = format(x_axis_dates, "%m/%d/%Y"), las = 1)


###########################
#task4:
#--For (non-weighted) historical simulation, estimate the α-VaR and ES for each asset from the empirical distribution of all log-returns. 
#--Again, set α = 0.95. 
#--Compare the results to RiskMetrics.

a<-0.95
log_returns<-log_returns_plot
a_Var_hist<-quantile(log_returns, probs = a, type = 1)  #standard term as non weighted!

a_Var_hist

#ES_a
#With proposition 1.8

log_returns_greater_a_Var<-log_returns[log_returns >= a_Var_hist]
#distribution info:
#summary(log_returns_greater_a_Var)

a_ES_hist<-mean(log_returns_greater_a_Var)
a_ES_hist

#values a bit higher for historical, but same scale...

###########################
#task5:
#--Perform backtesting studies for the three assets and the VaRs based on RiskMetrics and historical simulation. 
#--For the S&P 500 time series, use an estimation window with 1000 log-returns to estimate the 0.95-VaRs to predict the risk for the following day. 
#--Determine the violation ratios for the data (starting with the 1001th observation up to the last observation). 
#--For (non-weighted) historical simulation, use two variants in that the quantiles are estimated over the estimation window only, or over the whole past, respectively. 
#--Illustratethe time series of VaRs based on RiskMetrics.
#--Perform analogous studies for the Apple and Microsoft stocks. Use an estimation window with 500 days in these cases.
a<-0.95
est_win<-500

log_returns<-log_returns_plot

#########
#risk-metrics

#parameters
l<-0.94

#new df
d_rm<-data.frame(dates = dates_plot, log_returns = log_returns_plot, sqr_log_returns=sq_log_returns_plot)

########
#sigma-sq:

d_rm$sigma_sq <- NA

# Define the first value of C
d_rm$sigma_sq[1] <- d_rm$sqr_log_returns[1]

# Loop to fill in the rest of the column
for (i in 2:nrow(d_rm)) {
  d_rm$sigma_sq[i] <- (1-l)*d_rm$sqr_log_returns[i] + l*d_rm$sigma_sq[i - 1]
}

# View the updated data frame
head(d_rm)

######

#conditional VAR 
d_rm$cond_var <- NA

for (i in 501:nrow(d_rm)) {
  sig<-d_rm$sqr_log_returns[i-500]
  
  for (j in 1:499){
    sig<-l*sig + (1-l)*d_rm$sqr_log_returns[i-500+j]
  }	
  
  d_rm$cond_var[i] <- qnorm(a)*sqrt(sig)
  sig<-0
}


#####
#exceedences

d_rm$exce <- NA

for (i in 501:nrow(d_rm)) {
  if ( d_rm$log_returns[i] > d_rm$cond_var[i]) {
    # Code to execute if condition is TRUE
    d_rm$exce[i]=1
  } else {
    # Code to execute if none of the above conditions are TRUE
    d_rm$exce[i]=0
  }
}

#violation ratio:
vio_rat_risk_met <-sum(d_rm$exce[501:nrow(d_rm)])/( length(d_rm$exce[501:nrow(d_rm)]) * (1-a) )

vio_rat_risk_met

#####
#Illustratethe time series of VaRs based on RiskMetrics.

plot(d_rm$dates[501:nrow(d_rm)], d_rm$cond_var[501:nrow(d_rm)], type = "p", col = "black", 
     xlab = "Date", ylab = "0.95-VaR", 
     main = "Backtesting Vars with riskmetrics", xaxt = "n") 

# Define the specific dates for the x-axis
x_axis_dates <- c(seq(as.Date(d_rm$dates[501]), as.Date("2024-10-25"), by = "12 month"), as.Date("2024-10-25"))

# Add the custom x-axis
axis(1, at = x_axis_dates, labels = format(x_axis_dates, "%m/%d/%Y"), las = 1)


######################################
#historical

#parameters
a<-0.95
est_win<-500

log_returns<-log_returns_plot

#new df
d_hist<-data.frame(dates = dates_plot, log_returns = log_returns_plot, sqr_log_returns=sq_log_returns_plot)




#VAR 
d_hist$var_est <- NA
d_hist$var_tot <- NA

for (i in 501:nrow(d_hist)) {
  
  d_hel <- d_hist$log_returns[(i-500):(i-1)]
  d_hist$var_est[i] <- quantile(d_hel, probs = a, type = 1)  #standard term as non weighted!
  d_hel <- d_hist$log_returns[1:(i-1)]
  d_hist$var_tot[i] <- quantile(d_hel, probs = a, type = 1)  #standard term as non weighted!
  
}


#####
#exceedences

d_hist$exce_est <- NA
d_hist$exce_tot <- NA

for (i in 501:nrow(d_hist)) {
  if (d_hist$log_returns[i] > d_hist$var_est[i]) {
    # Code to execute if condition is TRUE
    d_hist$exce_est[i]=1
  } else {
    # Code to execute if none of the above conditions are TRUE
    d_hist$exce_est[i]=0
  }
  
  if (d_hist$log_returns[i] > d_hist$var_tot[i]) {
    # Code to execute if condition is TRUE
    d_hist$exce_tot[i]=1
  } else {
    # Code to execute if none of the above conditions are TRUE
    d_hist$exce_tot[i]=0
  }
}

#violation ratio:
vio_rat_hist_est <-sum(d_hist$exce_est[501:nrow(d_hist)])/( length(d_hist$exce_est[501:nrow(d_hist)]) * (1-a) )
vio_rat_hist_tot <-sum(d_hist$exce_tot[501:nrow(d_hist)])/( length(d_hist$exce_tot[501:nrow(d_hist)]) * (1-a) )

vio_rat_hist <- c(vio_rat_hist_est,vio_rat_hist_tot)
vio_rat_hist


#####
#troubles:
#d_hel <- d_hist$log_returns[2:501]
#d_hist$var_est[502] <- quantile(d_hel, probs = a, type = 1)

