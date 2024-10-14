# Exam_1 (10/13/24)
#### task_1 ####

# read the cleaned_covid_data csv file into RStudio 
cleaned_covid_data <- read.csv("cleaned_covid_data.csv")

View(cleaned_covid_data)

str(cleaned_covid_data)

#### task_2 ####

library(tidyverse)

# subset the data to just show states that begin with "A"
A_states <- cleaned_covid_data |>
  filter(grepl("^A", Province_State, ignore.case = TRUE))

View(A_states)

#### task_3 ####

# change the data type for the Last_Update column to 'Date' 
A_states <- A_states |>
  mutate(Last_Update = as.Date(Last_Update))

str(A_states)

# calculate the minimum and maximum dates from the Last_Update column
min_date <- min(A_states$Last_Update, na.rm = TRUE)
max_date <- max(A_states$Last_Update, na.rm = TRUE)

# create the scatterplot with LOESS curves and facets for each state
A_states |>
  ggplot(aes(x = Last_Update, y = Deaths)) +
  geom_point(alpha = 0.1) +  # scatterplot with complete transparency (lots of points so not obvious)
  geom_smooth(method = "loess", se = FALSE, color = "lightgreen") +  # LOESS curve without standard error shading
  facet_wrap(~ Province_State, scales = "free") +  # separate facet for each state with free scales
  labs(title = "COVID-19 Deaths Over Time in A-States",
       x = "Date", 
       y = "Number of Deaths") +
  scale_x_date(limits = c(min_date, max_date),  # set limits from the earliest to latest year-month
               breaks = seq(from = min_date, to = max_date, by = "3 months"),  # breaks every 3 months
               date_labels = "%Y-%m") +  # format date labels 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))  # rotate x-axis labels for a cleaner look 

#### task_4 ####

# find the peak Case_Fatality_Ratio for each state
state_max_fatality_rate <- cleaned_covid_data |>
  group_by(Province_State) |> 
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) |> # ignore any NA values
  ungroup() |> # ungroup the data 
  arrange(desc(Maximum_Fatality_Ratio))  # arrange values in descending order

View(state_max_fatality_rate)

#### task_5 ####

# convert the Province_State column into factor levels by order of descending Maximum_Fatality_Ratio 
state_max_fatality_rate <- state_max_fatality_rate |>
  mutate(Province_State = factor(Province_State, 
                                 levels = Province_State[order(-Maximum_Fatality_Ratio)]))

# create a bar plot visualizing the Maximum_Fatality_Ratio by Province/State 
state_max_fatality_rate |>
  ggplot(aes(x = Province_State, y = Maximum_Fatality_Ratio)) +
  geom_col(fill = "skyblue") +
  labs(title = "Maximum Case Fatality Ratio by State",
       x = "Province/State",
       y = "Maximum Fatality Ratio") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5))

#### task_6_BONUS ####

# convert Last_Update to 'Date' data type 
cleaned_covid_data <- cleaned_covid_data |>
  mutate(Last_Update = as.Date(Last_Update))

# summarize cumulative deaths over time for the entire USA
cumulative_deaths <- cleaned_covid_data |>
  group_by(Last_Update) |>
  summarize(Total_Deaths = sum(Deaths, na.rm = TRUE)) |>  # ignore any NA values 
  ungroup()

# plot cumulative deaths over time
cumulative_deaths |>
  ggplot(aes(x = Last_Update, y = Total_Deaths)) +
  geom_line(color = "maroon", size = 1) + # using a line plot 
  labs(title = "Cumulative Deaths from COVID-19 in the U.S.",
       x = "Year\nApril 2020 - January 2022",
       y = "Total Deaths") +
  scale_x_date(date_labels = "%Y", 
               date_breaks = "1 year", 
               limits = as.Date(c("2020-01-12", "2022-01-28"))) +  # set x-axis to cover years (not months); the date for 2020 was pushed back earlier because it wouldn't show with the min date of 2020-04-12
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))
