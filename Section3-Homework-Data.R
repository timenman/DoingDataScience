#Data
revenue <- c(14574.49, 7606.46, 8611.41, 9175.41, 8058.65, 8105.44, 11496.28, 9766.09, 10305.32, 14379.96, 10713.97, 15433.50)
expenses <- c(12051.82, 5695.07, 12319.20, 12089.72, 8658.57, 840.20, 3285.73, 5821.12, 6976.93, 16618.61, 10054.37, 3803.96)

#Solution

monthly_profitBT <- revenue - expenses
tax <- 0.3
taxes <- monthly_profitBT * tax
monthly_profitAT <- monthly_profitBT - taxes
profit_margin <- (round(monthly_profitAT / revenue, 2) *100)
mean_profit_margin <- mean(profit_margin)
best_month <- which(monthly_profitAT == max(monthly_profitAT))
worst_month <- which(monthly_profitAT == min(monthly_profitAT))
bad_months <- which(profit_margin < mean_profit_margin)
good_months <- which(profit_margin > mean_profit_margin)
revenue.1000 <- round(revenue/ 1000,0)
expenses.1000 <- round(expenses/ 1000,0)
monthly_profitBT.1000 <- round(monthly_profitBT/1000,0)


monthly_profitBT.1000
expenses.1000
revenue.1000
good_months
bad_months
worst_month
best_month
mean_profit_margin 
