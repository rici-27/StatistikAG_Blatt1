# Importing the Data 
# Plotting the time series of Log-Returns
# 

library(ggplot2)
library(readr)
library(tidyverse)

setwd("~/Desktop/Statistik_AG/Blatt1")

# Apple Daten einlesen
AppleData <- read_csv("AppleData.csv", 
    col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
    `Close/Last` = col_number(), 
    Volume = col_number(), 
    Open = col_number(), High = col_number(), 
    Low = col_number()))

# Closing Prices extrahieren
CloseData <- subset(AppleData, select = c("Date", "Close/Last"))

# LogReturns ausrechnen und in DataFrame einfügen
logreturns_vec <- diff(log(as.vector(CloseData$"Close/Last")))
n <- length(logreturns_vec)
# Hierfür letztes Datum entfernen
CloseData <- CloseData[1:n,]
CloseData$LogReturns <- logreturns_vec
View(CloseData)
# CloseData wegwerfen
CloseData$`Close/Last` <- NULL
View(CloseData)

# Time Series plotten
Close_TS_Plot <- ggplot(CloseData, aes(x = Date, y = LogReturns)) +
  geom_line(color = "darkgreen", alpha = 0.5)
ggsave(Close_TS_Plot,
       filename = "A_Close_TS.pdf",
       device = "pdf")
print(Close_TS_Plot)

# Squared LogReturns 
CloseData$Squared <- NA
CloseData$Squared <- CloseData$LogReturns**2

# Autokorrelation
pdf(file = "A_Aut_LogReturns.pdf",   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 4)
acf(CloseData$LogReturns)
dev.off()

pdf(file = "A_Aut_Squared_LogReturns.pdf",   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 4)
acf(CloseData$Squared)
dev.off()

# RiskMetrics Model & Plot
CloseData$Sigma <- NA
CloseData$Sigma[1] <- CloseData$LogReturns[1]**2
for (i in (2:n)){
  CloseData$Sigma[i] = 0.94 * (CloseData$Sigma[i-1]) + (1-0.94) * CloseData$LogReturns[i]**2
}
View(CloseData)


Volatility_Plot <- ggplot(CloseData, aes(x = Date, y = Sigma)) +
  geom_line()
ggsave(Volatility_Plot,
       filename = "A_Volatility.pdf",
       device = "pdf")

# Jetzt können wir die Werte einfach ausrechnen
VaR1 <- AppleData$`Close/Last`[n] * qnorm(0.95) * sqrt(CloseData$Sigma[n])
VaR1_Prozent <- qnorm(0.95) * sqrt(CloseData$Sigma[n])
## (hier kommt es drauf an ob wir den alpha-%-VaR suchen oder den von R_t)
ES1 <- (CloseData$Sigma[n] * dnorm(qnorm(0.95))) / (1-0.95)

# Erstmal die Empirische Verteilungsfunktion plotten
Empirische_F_Plot <-  ggplot(CloseData, aes(LogReturns)) + stat_ecdf(geom = "step")
ggsave(Empirische_F_Plot,
       filename = "A_Empirische_F.pdf",
       device = "pdf")

Fn <- ecdf(CloseData$LogReturns)
summary(Fn)
Var2 <- quantile(CloseData$LogReturns, 0.95)
# ?? hier bin ich mir jetzt unsicher mit den VZ und wie man weiter rechnet




