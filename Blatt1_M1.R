library(ggplot2)
library(readr)
library(tidyverse)

#setwd("~/Desktop/Statistik_AG/Blatt1")
#setwd("C:/Users/Anwender/Desktop/Statistik AG/Blatt1")

print("test")

# Microsoft Daten einlesen
MicrosoftData <- read_csv("MicrosoftData.csv", 
                      col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                       `Close/Last` = col_number(), 
                                       Volume = col_number(), 
                                       Open = col_number(), High = col_number(), 
                                       Low = col_number()))
View(MicrosoftData)

## data frame einmal umdrehen damit daten aufsteigen

MicrosoftData <- MicrosoftData %>% arrange(Date)
View(MicrosoftData)

# Closing Prices extrahieren
CloseData <- subset(MicrosoftData, select = c("Date", "Close/Last"))

# LogReturns ausrechnen und in DataFrame einfügen
# genauer habe ich LogReturn = R_t = ln(P_{t+1}) - ln(P_t)
logreturns_vec <- diff(log(as.vector(CloseData$"Close/Last")))
n <- length(logreturns_vec)
# Hierfür letztes Datum entfernen
CloseData <- CloseData[1:n,]
CloseData$LogReturns <- logreturns_vec
rm(logreturns_vec)
View(CloseData)
# CloseData wegwerfen
# CloseData$`Close/Last` <- NULL

# Time Series plotten
Close_TS_Plot <- ggplot(CloseData, aes(x = Date, y = LogReturns)) +
  geom_line(color = "darkgreen", alpha = 0.5)
ggsave(Close_TS_Plot,
       filename = "M_Close_TS.pdf",
       device = "pdf")
print(Close_TS_Plot)

# Squared LogReturns 
CloseData$Squared <- NA
CloseData$Squared <- CloseData$LogReturns**2

# Autokorrelation
pdf(file = "M_Aut_LogReturns.pdf",   # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 4)
acf(CloseData$LogReturns)
dev.off()

pdf(file = "M_Aut_Squared_LogReturns.pdf",   # The directory you want to save the file in
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
rm(i)
View(CloseData)


Volatility_Plot <- ggplot(CloseData, aes(x = Date, y = Sigma)) +
  geom_line()
ggsave(Volatility_Plot,
       filename = "M_Volatility.pdf",
       device = "pdf")

# Jetzt können wir die Werte einfach ausrechnen
VaR_RM <- qnorm(0.95) * sqrt(CloseData$Sigma[n])
## (hier kommt es drauf an ob wir den alpha-%-VaR suchen oder den von R_t)
ES_RM <- (sqrt(CloseData$Sigma[n]) * dnorm(qnorm(0.95))) / (1-0.95)

# Erstmal die Empirische Verteilungsfunktion plotten
Empirische_F_Plot <-  ggplot(CloseData, aes(LogReturns)) + stat_ecdf(geom = "step")
ggsave(Empirische_F_Plot,
       filename = "M_Empirische_F.pdf",
       device = "pdf")

## Verteilungsfunktion von LogReturns
Fn <- ecdf(CloseData$LogReturns)
summary(Fn)
Var_H <- unname(quantile(CloseData$LogReturns, 0.95))
rm(Fn)
ES_H <- mean(CloseData$LogReturns > Var_H)


## Backtesting
## RiskMetrics
View(CloseData)
CloseData2 <- CloseData
CloseData2$Sigma <- NULL
CloseData2$Squared <- NULL
View(CloseData2)

CloseData2$Var_RM <- NA

# Funktion schreiben die Var_{t|t-1} zurückgibt
VarRechner_RM <- function(k){
  CloseData2$Sigma <- NA
  CloseData2$Sigma[k-500] = CloseData2$`Close/Last`[k-500]**2
  for (i in ((k-499):(k))){
    CloseData2$Sigma[i] <- 0.94 * (CloseData2$Sigma[i-1]) + (1-0.94) * CloseData2$LogReturns[i]**2 
  }
    return(sqrt(CloseData2$Sigma[k]) * qnorm(0.95))
}

# Jetzt Funktion für 501 bis n (letzer Wert) aufrufen
for (m in (501:n)){
  CloseData2$Var_RM[m] <- VarRechner_RM(m)
}
View(CloseData2)

# Jetzt Anzahl an exceedances ausrechnen
exceedances_RM = 0
for (m in (501:n)){
  if (CloseData2$LogReturns[m] > CloseData2$Var_RM[m]) {
    exceedances_RM <- exceedances_RM + 1
  }
}
VioR_RM <- exceedances_RM/ ((1-0.95) * (n-500))
rm(m)

## Historical Simulation Variante 1 (estimation window only)
# neue Spalte anlegen
CloseData2$Var_H1 <- NA
View(CloseData2)

# Var_H1 ausrechnen
for (l in (501:n)){
  CloseData2$Var_H1[l] <- unname(quantile(CloseData2$LogReturns[(l-500):(l-1)], 0.95))
}
View(CloseData2)

# exceedances_H1
exceedances_H1 = 0
for (m in (501:n)){
  if (CloseData2$LogReturns[m] > CloseData2$Var_H1[m]) {
    exceedances_H1 <- exceedances_H1 + 1
  }
}
VioR_H1 <- exceedances_H1/ ((1-0.95) * (n-500))

## Historical Simulation Variante 2 (ganze Vergangenheit als est window)

CloseData2$Var_H2 <- NA
View(CloseData2)

# Var_H2 ausrechnen
for (l in (501:n)){
  CloseData2$Var_H2[l] <- unname(quantile(CloseData2$LogReturns[1:(l-1)], 0.95))
}
View(CloseData2)

# exceedances_H2
exceedances_H2 = 0
for (m in (501:n)){
  if (CloseData2$LogReturns[m] > CloseData2$Var_H2[m]) {
    exceedances_H2 <- exceedances_H2 + 1
  }
}
VioR_H2 <- exceedances_H2/ ((1-0.95) * (n-500))

## unnötige Variablen entfernen
rm(l, m, n)

