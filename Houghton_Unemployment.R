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
plot(ts(resid(model1), start = c(2011, 1), freq = 12)) # time-series plot of residuals from regression model
abline(h = 0, col = 2)
plot(rstandard(model1)) # standard residual plot
abline(h = 0, col = 2)

qqnorm(resid(model1)) # plot to check normality
qqline(resid(model1))

shapiro.test(resid(model1)) # Shapiro-Wilk test (homoscedasticity)
runs.test(resid(model1)) # runs test (independence)

acf(resid(model1)) # ACF plot (independence)

# Seasonal means model --------------------------------------------------------
unemployment_ts <- ts(unemployment$MIHOUG1URN, freq = 12)
month <- season(unemployment_ts)
model2 <- lm(unemployment_ts ~ month)

lx1 <- ts(resid(model2), start = c(2011, 1), freq = 12)
plot(lx1)

AIC(model2)
AICc(model2)
BIC(model2)

# Cosine trends model ---------------------------------------------------------
har <- harmonic(unemployment_ts, 1)
model3 <- lm(unemployment_ts ~ har)
summary(model3)

AIC(model3)
AICc(model3)
BIC(model3)

# SARIMA ----------------------------------------------------------------------
tm <- time(unemployment_ts)
tm2 <- time(unemployment_ts)^2
model0 <- lm(unemployment_ts ~ tm + tm2)
lx0 <- ts(resid(model0), start = c(2011, 1), freq = 12)
plot(lx0)

acf(lx0)
pacf(lx0)

# Select candidates for p,q,d -------------------------------------------------
model0_sarima <- arima(lx0, order = c(1, 0, 0), seasonal = list(order = c(0, 1, 1)))

auto.arima(lx0)

# Model Selection -------------------------------------------------------------


# Final Model Diagnostics -----------------------------------------------------
tsdiag(model0_sarima)

qqnorm(resid(model0_sarima)) # plot to check normality
qqline(resid(model0_sarima))

shapiro.test(resid(model0_sarima)) # Shapiro-Wilk test (homoscedasticity)
runs.test(resid(model0_sarima)) # runs test (independence)

acf(resid(model0_sarima)) # ACF plot (independence)
