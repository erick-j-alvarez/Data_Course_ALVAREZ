# exam_2 
library(tidyverse)
library(janitor)
library(easystats)

#### 1) read in the unicef data ####

# read in the original unicef data 
unicef_dat <- read.csv("./unicef-u5mr.csv")

# investigate structure of data frame 
str(unicef_dat)

#### 2) get it into tidy format ####

# convert the column titles to snake case format 
clean_names_unicef <- janitor::clean_names(unicef_dat)

# convert the data frame from wide to long format
clean_names_unicef_long <- clean_names_unicef |>
  pivot_longer(
    cols = starts_with("u5mr_"),
    names_to = "year",
    values_to = "u5mr"
  ) |>
  mutate(
    year = as.numeric(sub("u5mr_", "", year)) # remove the "u5mr_" prefix and convert to numeric
  )

# double check the data types and data structure 
str(clean_names_unicef_long)

#### 3) Plot each country’s U5MR over time ####

# line plot for each country's U5MR over time, faceted by continent
ALVAREZ_Plot_1 <- clean_names_unicef_long |>
  ggplot(aes(x = year, y = u5mr, group = country_name)) +
  geom_line() + 
  facet_wrap(~ continent) + 
  labs(
    title = "Under-5 Mortality Rate (U5MR) Over Time by Country",
    x = "Year",
    y = "U5MR (per 1000 live births)"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 10, face = "bold")  # facet labels style
  )

# view the above plot 
print(ALVAREZ_Plot_1)

#### 4) Save this plot as LASTNAME_Plot_1.png ####

# save the plot as a .png file 
ggsave("ALVAREZ_Plot_1.png", plot = ALVAREZ_Plot_1, width = 10, height = 8, dpi = 300)

#### 5) Create another plot that shows the mean U5MR for all the countries within a given continent at each year ####

# calculate the mean u5mr for each continent by year
mean_u5mr_continent <- clean_names_unicef_long |>
  group_by(continent, year) |>
  summarize(mean_u5mr = mean(u5mr, na.rm = TRUE))  # calculate mean excluding NAs

# plot mean u5mr for each continent over time
ALVAREZ_Plot_2 <- mean_u5mr_continent |>
  ggplot(aes(x = year, y = mean_u5mr, color = continent, group = continent)) +
  geom_line(size = 1.5) + # make the lines thicker
  labs(
    title = "Mean Under-5 Mortality Rate (U5MR) Over Time by Continent",
    x = "Year",
    y = "Mean U5MR (per 1000 live births)"
  ) +
  theme_bw() +
  theme(
    legend.position = "right",  # show legend on the right of the plot 
    plot.title = element_text(size = 14, face = "bold"), 
    axis.title = element_text(size = 12)  
  )

# view the above plot 
print(ALVAREZ_Plot_2)

#### 6) Save that plot as LASTNAME_Plot_2.png ####

# save the plot as a .png file 
ggsave("ALVAREZ_Plot_2.png", plot = ALVAREZ_Plot_2, width = 10, height = 8, dpi = 300)

#### 7) Create three models of U5MR ####

# model 1: u5mr accounted for only by Year
mod1 <- lm(u5mr ~ year, data = clean_names_unicef_long)

# model 2: u5mr accounted for by Year and Continent
mod2 <- lm(u5mr ~ year + continent, data = clean_names_unicef_long)

# model 3: u5mr accounted for by Year, Continent, and their interaction term
mod3 <- lm(u5mr ~ year * continent, data = clean_names_unicef_long)

# summarize each model using the easystats package (model_parameters function)
summary_mod1 <- model_parameters(mod1)
summary_mod2 <- model_parameters(mod2)
summary_mod3 <- model_parameters(mod3)

# print the summaries 
print(summary_mod1)
print(summary_mod2)
print(summary_mod3)

#### 8) Compare the three models with respect to their performance ####

# compare the models with performance metrics
model_comparison <- compare_parameters(mod1, mod2, mod3)

# print model comparison
print(model_comparison)

# plot the comparison
model_comparison |>
  plot()

# AIC comparison
aic_values <- c(AIC(mod1), AIC(mod2), AIC(mod3))
names(aic_values) <- c("mod1", "mod2", "mod3")
print(aic_values)

# BIC comparison
bic_values <- c(BIC(mod1), BIC(mod2), BIC(mod3))
names(bic_values) <- c("mod1", "mod2", "mod3")
print(bic_values)

# R-squared comparison
rsq_values <- c(summary(mod1)$r.squared, summary(mod2)$r.squared, summary(mod3)$r.squared)
names(rsq_values) <- c("mod1", "mod2", "mod3")
print(rsq_values)

# which model is best? 
# according to Google (a lot of it):
  # AIC (Akaike Information Criterion): Helps compare models by considering both how well they fit the data and how complex they are; lower values indicates a better balance.
  # BIC (Bayesian Information Criterion): Helps compared models by considering both how well they fit and how complex they are, but with a stronger penalty for complexity (whatever that means); lower values indicate a more efficient model.
  # R-squared: Measures how well the model explains the variation in the outcome; higher values mean the model explains more of the data.
# according to the above parameters that I found online, and the model_comparison plot, mod3 was the best model since it had the lowest BIC and AIC scores, the highest R-squared score, and displayed more of the data on the model_comparison plot.

#### 9) Plot the 3 models’ predictions like so: ####

# generate predictions for each model based on year 
pred_mod1 <- predict(mod1, clean_names_unicef_long)
pred_mod2 <- predict(mod2, clean_names_unicef_long)
pred_mod3 <- predict(mod3, clean_names_unicef_long)

# add predictions to the data frame
clean_names_unicef_long$pred_mod1 <- pred_mod1
clean_names_unicef_long$pred_mod2 <- pred_mod2
clean_names_unicef_long$pred_mod3 <- pred_mod3

# create a data frame summarizing the predictions by year, continent, and model
predictions_long <- clean_names_unicef_long |>
  select(year, continent, pred_mod1, pred_mod2, pred_mod3) |>
  gather(key = "model", value = "predicted_u5mr", -year, -continent)

# plot the predicted models faceted by model 
ALVAREZ_Plot_3 <- predictions_long |>
  ggplot(aes(x = year, y = predicted_u5mr, color = continent, group = continent)) +
  geom_line(size = 1.2) +  # slightly thicker lines 
  facet_wrap(~ model, scales = "free_y") + # with independent y-axis
  labs(title = "Predicted U5MR Over Time by Model and Continent", 
       x = "Year", 
       y = "Predicted U5MR (per 1000 live births)") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "right"  # position legend to the right of the plot 
  )

# print the plot above 
print(ALVAREZ_Plot_3)

# save the plot as a .png file 
ggsave("ALVAREZ_Plot_3.png", plot = ALVAREZ_Plot_3, width = 10, height = 8, dpi = 300)
