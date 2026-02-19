# Load libraries 
library(car)
library(lmtest)
library(ggplot2)

# Remove old objects 
rm(list = c("Y","X2","X3","X5"))

# Attach dataset
attach(mult_reg_xls)


# Assign variables
Y  <- wages
X2 <- educ
X3 <- training
X5 <- as.factor(Gender)

# Estimate interaction model
model1.4 <- lm(wages ~ educ + training + Gender +
                 educ:Gender + training:Gender)

summary(model1.4)

# VIF 
vif(model1.4, type = "predictor")

# Correlation matrix 
cor(mult_reg_xls[, sapply(mult_reg_xls, is.numeric)])

# Breusch–Pagan test
bptest(model1.4)

# White test
bptest(model1.4,
       ~ fitted(model1.4) +
         I(fitted(model1.4)^2))

# Breusch–Godfrey test
bgtest(model1.4, order = 1)

# RESET test
resettest(model1.4)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(model1.4)

# Residual vs Fitted
plot(fitted(model1.4), resid(model1.4))
abline(h = 0)
