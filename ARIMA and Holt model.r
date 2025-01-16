library(quantmod)
library(forecast)
library(rugarch)
library(TSA)

# Set symbol lookup and get stock data
setSymbolLookup(STOCK = list(name = "0941.HK", src = "yahoo"))
getSymbols("STOCK", from = "2023-02-20")
STOCK_CP <- Cl(STOCK)

# ARIMA model
ARIMA <- auto.arima(STOCK_CP, allowdrift = FALSE, seasonal = TRUE)
price_forecast_ARIMA <- forecast(ARIMA, h = 18)
plot(price_forecast_ARIMA)
checkresiduals(price_forecast_ARIMA)

# ARFIMA model
fitarfima <- autoarfima(data = STOCK_CP, criterion = "AIC", method = "full")
print(fitarfima)

# GARCH(1,1) model
garch11_spec <- ugarchspec(variance.model = list(garchOrder = c(1, 1)), mean.model = list(armaOrder = c(2, 2)))
garch11_fit <- ugarchfit(spec = garch11_spec, data = STOCK_CP)
plot(sigma(garch11_fit), ylab = "sigma(t)", col = "blue")

# Residuals analysis
garch_res <- residuals(garch11_fit, standardize = TRUE)
qqnorm(garch_res)
qqline(garch_res)
Box.test(garch_res^2, type = "Ljung-Box")

# GARCH forecast
garch_forecast <- ugarchforecast(garch11_fit, n.ahead = 18)
plot(garch_forecast)
print(garch_forecast)

# Holt's linear trend model
getSymbols("STOCK", from = "2020-12-28")
holt_CP <- Cl(STOCK)
holt_train <- window(holt_CP, end = 600)
holt_test <- window(holt_CP, start = 601)

beta <- seq(0.0001, 0.5, by = 0.001)
RMSE <- sapply(beta, function(b) {
  fit <- holt(holt_train, beta = b, h = 100)
  accuracy(fit, holt_test)[2, 2]
})

beta_min <- beta[which.min(RMSE)]
holt_opt <- holt(holt_CP, h = 100, beta = beta_min)
autoplot(holt_opt)
checkresiduals(holt_opt)
holt_forecast_opt <- forecast(holt_opt, h = 18)
plot(holt_forecast_opt)
checkresiduals(holt_forecast_opt)
