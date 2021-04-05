# Data Importing from CSV -----------------------------------------------------
unemployment <- read_csv("~/GitHub/Time-Series-Unemployment/MIHOUG1URN.csv", 
                       col_types = cols(DATE = col_date(format = "%Y-%m-%d")))

# Preliminary Plotting --------------------------------------------------------
unemployment$DATE <- as.Date(unemployment$DATE) #convert dates from "character" type to "date" type
plot(unemployment,main="Monthly Unemployment Rates in Houghton County",
     xlab="Time",ylab="Unemployment Rate")
plot(diff(unemployment$MIHOUG1URN), main="Plot of Differences",
     ylab="Differences")

# Deterministic Trend: Assumption Checking ------------------------------------
model1 <- lm(unemployment$MIHOUG1URN ~ unemployment$DATE) #linear regression model
plot(ts(resid(model1),start=c(2011,1),freq=12)) #time-series plot of residuals from regression model
plot(rstandard(model1)) #standard residual plot
abline(h=0,col=2)

qqnorm(resid(model1)) #plot to check normality
qqline(resid(model1))

shapiro.test(resid(model1)) #Shapiro-Wilk test (normality) and runs test (independence)
runs(resid(model1))

acf(resid(model1)) #ACF plot (independence)


# Transformation if necessary -------------------------------------------------


# Select candidates for p,q,d -------------------------------------------------


# Estimate coefficients -------------------------------------------------------


# Model Selection -------------------------------------------------------------


# Final Model Diagnostics -----------------------------------------------------


# Forecasting (optional)


# Undo Previous Transformation ------------------------------------------------