# Var1 (RiskMetrics) nur mit den ersten 500 Einträgen bestimmen
Var1_500 <- sqrt(CloseData$Sigma[500]) * qnorm(0.95)

# Var2 (HistoricalSimulation) nur mit den ersten 500 Einträgen bestimmen
Var2_500 <- quantile(CloseData$LogReturns[1:500], 0.95)

# 