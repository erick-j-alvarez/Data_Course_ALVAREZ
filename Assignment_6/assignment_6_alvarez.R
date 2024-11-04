# assignment_6
library(tidyverse)
library(janitor)
library(gganimate)

# read in the BioLog_Plate_Data.csv untidy data 
dat <- read_csv("../Data/BioLog_Plate_Data.csv")

str(dat)

# clean column names of the data 
nice_names_dat <- clean_names(dat)

# determining unique sample_id categories 
unique(nice_names_dat$sample_id)

# two lists consisting of the sample_id categories 
water_types <- c('Clear_Creek', 'Waste_Water')
soil_types <- c('Soil_1', 'Soil_2')

# create a new column that distinguishes between soil and water 
new_column_dat <- nice_names_dat |>
  mutate(sample_type = case_when(
    sample_id %in% water_types ~ 'Water',
    sample_id %in% soil_types ~ 'Soil'
  ))

# convert to longer format by splitting up "absorbance" and "time"
long_completed_dat <- new_column_dat |>
  pivot_longer(cols = starts_with("hr_"),       # select columns that start with "hr_"
               names_to = "time",               # create a new column named "time"
               values_to = "absorbance") |>     # create a new column named "absorbance" to store float values
  mutate(time = as.numeric(str_remove(time, "hr_")))  # remove "hr_" prefix and convert to numeric

# create the faceted line plot while filtering for diluted samples equal to 0.1
time_absorbance_plot <- long_completed_dat |>
  filter(dilution == 0.1) |>  # filter dilutions equal to 0.1 
  ggplot(aes(x = time, y = absorbance, color = sample_type)) +  # set color by sample_type
  geom_smooth(se = FALSE) +                                     # remove shaded error 
  facet_wrap(~substrate) +                                      # facet by substrate
  labs(title = "Just dilution 0.1", x = "Time", y = "Absorbance", color = "Type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0))  # align the title to the left

# print the static plot 
print(time_absorbance_plot)

# filter the data for only Itaconic Acid substrate
itaconic_data <- long_completed_dat |>
  filter(substrate == "Itaconic Acid")

# calculate mean absorbance for each dilution and rep group
mean_absorbance <- itaconic_data |>
  group_by(sample_id, dilution, time) |>
  summarize(mean_absorbance = mean(absorbance, na.rm = TRUE), .groups = 'drop')

# create the animated plot 
animated_plot <- mean_absorbance |>
  ggplot(aes(x = time, y = mean_absorbance, color = sample_id, group = sample_id)) +
  geom_line() +
  facet_wrap(~ dilution) +  # facet by dilution 
  labs(title = 'Mean Absorbance over Time for Itaconic Acid by Dilution',
       x = 'Time',
       y = 'Mean Absorbance',
       color = 'Sample ID') +
  theme_minimal() +
  transition_reveal(time)  # animate over time 

# print the animated plot 
print(animated_plot)
