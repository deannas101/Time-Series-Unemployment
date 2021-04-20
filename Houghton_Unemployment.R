library(AICcmodavg)
library(forecast)
library(TSA)

# Data Importing from CSV -----------------------------------------------------
unemployment <- read_csv("~/GitHub/Time-Series-Unemployment/MIHOUG1URN.csv",
  col_types = cols(DATE = col_date(format = "%Y-%m-%d"))
)

# Preliminary Plotting --------------------------------------------------------
unemployment$DATE <- as.Date(unemployment$DATE) # convert dates from "character" type to "date" type
plot(unemployment,
  main = "Monthly Unemployment Rates in Houghton County",
  xlab = "Time", ylab = "Unemployment Rate"
)
plot(diff(unemployment$MIHOUG1URN),
  main = "Plot of Differences",
  ylab = "Differences"
)
abline(h = 0)

# Deterministic Trend: Assumption Checking ------------------------------------
model1 <- lm(unemployment$MIHOUG1URN ~ unemployment$DATE) # linear regression model
plot(rstandard(model1), type="l", main = "Time Series Residuals") # time series of residuals
abline(h = 0, col = 2)
hist(resid(model1), main = "Histogram of Residuals")
summary(model1)

qqnorm(resid(model1)) # plot to check normality
qqline(resid(model1))

shapiro.test(resid(model1)) # Shapiro-Wilk test (homoscedasticity)
runs(resid(model1)) # runs test (independence)

acf(resid(model1)) # ACF plot (independence)

# Seasonal means model --------------------------------------------------------
unemployment_ts <- ts(unemployment$MIHOUG1URN, freq = 12)
month <- season(unemployment_ts)
tm <- time(unemployment_ts)
model2 <- lm(unemployment_ts ~ month + tm)

lx1 <- ts(resid(model2), start = c(2011, 1), freq = 12)
plot(lx1, main = "Residual Time Series at Lag 1")

summary(model2)

# Cosine trends model ---------------------------------------------------------
har <- harmonic(unemployment_ts, 1)
model3 <- lm(unemployment_ts ~ har + tm)
lx1 <- ts(resid(model3), start = c(2011, 1), freq = 12)
plot(lx1, main = "Residual Time Series at Lag 1")
summary(model3)

# SARIMA ----------------------------------------------------------------------
tm <- time(unemployment_ts)
model0 <- lm(unemployment_ts ~ tm)
lx0 <- ts(resid(model0), start = c(2011, 1), freq = 12)
plot(lx0)

acf(lx0, lag.max=30)
pacf(lx0, lag.max=30)

# Select candidates for p,q,d -------------------------------------------------
auto.arima(lx0)

# Final Model Diagnostics -----------------------------------------------------
qqnorm(resid(model2)) # plot to check normality
qqline(resid(model2))

shapiro.test(resid(model2)) # Shapiro-Wilk test (homoscedasticity)
runs(resid(model2)) # runs test (independence)

acf(resid(model2), lag.max=30) # ACF plot (independence)

# trying to plot model over data
modelt <- lm(unemployment_ts ~ as.numeric(tm))
plot(unemployment,
     main = "Monthly Unemployment Rates in Houghton County",
     xlab = "Time", ylab = "Unemployment Rate"
)
abline(modelt, lty=1,col="black")
