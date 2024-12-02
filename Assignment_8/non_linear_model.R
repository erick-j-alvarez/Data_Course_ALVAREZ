# Load the non-linear data 
df <- read.csv("../Data/non_linear_relationship.csv")

# Fit a linear model with a quadratic polynomial term for predictor (plot came out similarly with a cubic polynomial term)
lm_poly_model <- lm(response ~ poly(predictor, 2), data = df)

# Summarize the model
summary(lm_poly_model)

# Plot the polynomial fit (due to simplicity, used base R instead of ggplot2)
plot(df$predictor, df$response, main="Predictor vs. Response", xlab="Predictor", ylab="Response", pch=19)