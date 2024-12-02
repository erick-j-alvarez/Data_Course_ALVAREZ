# assignment 8 (12/2/24)
library(modelr)
library(easystats)
library(broom)
library(tidyverse)
library(fitdistrplus)

#### 1) loads the “/Data/mushroom_growth.csv” data set ####
mushroom_dat <- read.csv("../Data/mushroom_growth.csv")

str(mushroom_dat)

#### 2) creates several plots exploring relationships between the response and predictors ####
# 1. Scatter plot: GrowthRate vs. Light (Numeric Predictor)
gr_light_scatter <- ggplot(mushroom_dat, aes(x = Light, y = GrowthRate)) +
  geom_point(aes(color = Species), size = 2) + # color points by Species
  labs(title = "Growth Rate vs. Light", x = "Light", y = "Growth Rate") +
  theme_minimal()

print(gr_light_scatter)

# 2. Scatter plot: GrowthRate vs. Nitrogen (Numeric Predictor)
gr_nitrogen_scatter <- ggplot(mushroom_dat, aes(x = Nitrogen, y = GrowthRate)) +
  geom_point(aes(color = Species), size = 2) + # color points by Species
  labs(title = "Growth Rate vs. Nitrogen", x = "Nitrogen", y = "Growth Rate") +
  theme_minimal()

print(gr_nitrogen_scatter)

# 3. Box plot: GrowthRate vs. Humidity (Categorical Predictor)
gr_humidity_box <- ggplot(mushroom_dat, aes(x = Humidity, y = GrowthRate, fill = Species)) +
  geom_boxplot() +
  labs(title = "Growth Rate by Humidity", x = "Humidity", y = "Growth Rate") +
  theme_minimal()

print(gr_humidity_box)

# 4. Box plot: GrowthRate vs. Temperature (Numeric Predictor)
gr_temp_box <- ggplot(mushroom_dat, aes(x = as.factor(Temperature), y = GrowthRate, fill = Species)) + # Temperature converted to a factor
  geom_boxplot() +
  labs(title = "Growth Rate by Temperature", x = "Temperature", y = "Growth Rate") +
  theme_minimal()

print(gr_temp_box)

# 5. Faceted scatter plots: GrowthRate vs. predictors; faceted by Species
gr_predictors_facet_scatter <- ggplot(mushroom_dat, aes(x = Light, y = GrowthRate)) +
  geom_point(aes(color = Nitrogen)) + 
  facet_wrap(~Species) + 
  labs(title = "Growth Rate vs. Light (Faceted by Species)", x = "Light", y = "Growth Rate") +
  theme_minimal()

print(gr_predictors_facet_scatter)

# 6. Visualizing interaction between Nitrogen and Light on GrowthRate (Heatmap)
gr_nitrogen_light_heatmap <- ggplot(mushroom_dat, aes(x = Light, y = Nitrogen, fill = GrowthRate)) +
  geom_tile() + 
  scale_fill_viridis_c() +
  labs(title = "Growth Rate Interaction: Light and Nitrogen", x = "Light", y = "Nitrogen", fill = "Growth Rate") +
  theme_minimal()

print(gr_nitrogen_light_heatmap)

#### 3) defines at least 4 models that explain the dependent variable “GrowthRate” ####
# 1. Simple Linear Model: GrowthRate as a function of Light
model1 <- lm(GrowthRate ~ Light, data = mushroom_dat)
summary(model1)

# 2. Multiple Linear Model: GrowthRate as a function of Light, Nitrogen, and Temperature
model2 <- lm(GrowthRate ~ Light + Nitrogen + Temperature, data = mushroom_dat)
summary(model2)

# 3. Model with Interaction: GrowthRate as a function of Light, Nitrogen, and their interaction
model3 <- lm(GrowthRate ~ Light * Nitrogen, data = mushroom_dat)  # Light:Nitrogen
summary(model3)

# 4. Model with Categorical Variable (Humidity): GrowthRate as a function of Light, Nitrogen, and Humidity
model4 <- lm(GrowthRate ~ Light + Nitrogen + Humidity, data = mushroom_dat)
summary(model4)

#### 4) calculates the mean sq. error of each model ####
# Calculate MSE (Mean Square Error) for model1
model1_mse <- mean(residuals(model1)^2)
cat("Model 1 Mean Squared Error (MSE):", model1_mse, "\n")

# Calculate MSE for model2
model2_mse <- mean(residuals(model2)^2)
cat("Model 2 Mean Squared Error (MSE):", model2_mse, "\n")

# Calculate MSE for model3
model3_mse <- mean(residuals(model3)^2)
cat("Model 3 Mean Squared Error (MSE):", model3_mse, "\n")

# Calculate MSE for model4
model4_mse <- mean(residuals(model4)^2)
cat("Model 4 Mean Squared Error (MSE):", model4_mse, "\n")

### 5) selects the best model you tried ####
# Store MSE values in a vector
mse_values <- c(Model1 = model1_mse, Model2 = model2_mse, Model3 = model3_mse, Model4 = model4_mse)

# Identify the best model (the smallest MSE value is the best)
best_model <- names(which.min(mse_values))
cat("\nThe best model is:", best_model, "with an Mean Square Error (MSE) of", min(mse_values), "\n")

#### 6) adds predictions based on new hypothetical values for the independent variables used in your model ####
# Predicting GrowthRate values using model4 (best MSE model)
# Uses the original data values 
prediction_df <- mushroom_dat |>
  add_predictions(model4) 

View(prediction_df)

# model4 formula: lm(GrowthRate ~ Light + Nitrogen + Humidity, data = mushroom_dat)
# Generate all combinations of Light, Nitrogen, and Humidity
newdf <- expand.grid(Light = c(5, 15, 30),
                     Nitrogen = c(6, 9, 12, 15, 18, 21, 24, 27, 33),
                     Humidity = c("Low", "High"))

# Change data types to match the original data frame types
newdf$Light <- as.integer(newdf$Light)
newdf$Nitrogen <- as.integer(newdf$Nitrogen)
newdf$Humidity <- as.character(newdf$Humidity)

# Predict GrowthRate based on newly implemented hypothetical values 
pred = predict(model4, newdata = newdf)

# Combining hypothetical input data with hypothetical predictions into one data frame
hyp_preds <- data.frame(Light = newdf$Light,
                        Nitrogen = newdf$Nitrogen,
                        Humidity = newdf$Humidity,
                        pred = pred)

# Add new column showing whether a data point is real or hypothetical
prediction_df$PredictionType <- "Real"
hyp_preds$PredictionType <- "Hypothetical"

# Joining our real data and hypothetical data (with model predictions)
fullpreds <- full_join(prediction_df,hyp_preds)

#### 7) plots these predictions alongside the real data ####
# Box Plot 1: Light vs Predicted GrowthRate
plot_light_box <- ggplot(fullpreds, aes(x = factor(Light), y = pred, fill = PredictionType)) +
  geom_boxplot() +
  labs(title = "Growth Rate Predictions by Light",
       x = "Light",
       y = "Predicted Growth Rate") +
  theme_minimal() +
  theme(legend.position = "top")

# Box Plot 2: Nitrogen vs Predicted GrowthRate
plot_nitrogen_box <- ggplot(fullpreds, aes(x = factor(Nitrogen), y = pred, fill = PredictionType)) +
  geom_boxplot() +
  labs(title = "Growth Rate Predictions by Nitrogen",
       x = "Nitrogen",
       y = "Predicted Growth Rate") +
  theme_minimal() +
  theme(legend.position = "top")

# Box Plot 3: Humidity vs Predicted GrowthRate
plot_humidity_box <- ggplot(fullpreds, aes(x = factor(Humidity), y = pred, fill = PredictionType)) +
  geom_boxplot() +
  labs(title = "Growth Rate Predictions by Humidity",
       x = "Humidity",
       y = "Predicted Growth Rate") +
  theme_minimal() +
  theme(legend.position = "top")

# Print the plots
print(plot_light_box)
print(plot_nitrogen_box)
print(plot_humidity_box)

