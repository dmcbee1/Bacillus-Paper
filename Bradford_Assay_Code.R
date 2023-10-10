# ==========================================
# INPUTS
# ==========================================

# Data for standards
concentrations <- c()#input concentrations of standards
absorbance_standard <- c()#input absorbance of each standard

# Absorbance values for treatments
treatments_B_absorbance <- c()
treatments_C_absorbance <- c()

# ==========================================
# STANDARD CURVE AND MODEL FITTING
# ==========================================

# Plotting the standard curve
plot(absorbance_standard, concentrations, main="Standard Curve with Regression Line", xlab="Absorbance", ylab="Protein Concentration", pch=19, col="blue", xlim=c(0, 2.5), ylim=c(0, 2100))

# Fit a linear regression model to the standard data
model <- lm(concentrations ~ absorbance_standard)
abline(model, col="red") # Add regression line to the plot

# Compute and display R squared value
r_squared <- summary(model)$r.squared
cat("R squared value:", r_squared, "\n")

# ==========================================
# PREDICTION FOR TREATMENTS
# ==========================================

# Predict protein concentrations for treatments using the regression model
predicted_B <- predict(model, newdata=data.frame(absorbance_standard=treatments_B_absorbance))
predicted_C <- predict(model, newdata=data.frame(absorbance_standard=treatments_C_absorbance))

cat("Predicted protein concentrations for treatments B:", predicted_B, "\n")
cat("Predicted protein concentrations for treatments C:", predicted_C, "\n")

# ==========================================
# CALCULATE MEAN AND STANDARD DEVIATION
# ==========================================

mean_B <- mean(predicted_B)
sd_B <- sd(predicted_B)
mean_C <- mean(predicted_C)
sd_C <- sd(predicted_C)

cat("Treatment B - Average:", mean_B, "Standard Deviation:", sd_B, "\n")
cat("Treatment C - Average:", mean_C, "Standard Deviation:", sd_C, "\n")

