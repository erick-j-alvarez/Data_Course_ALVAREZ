# assignment 7
library(tidyverse)
library(janitor)
library(dplyr)

#### 1) Import the Assignment_7/Utah_Religions_by_County.csv data set ####
# reading in the data set 
rel_by_county_dat <- read.csv("../Assignments/Assignment_7/Utah_Religions_by_County.csv")

#### 2) Clean it up into “tidy” shape ####
# forces decimal notation
options(scipen = 999)

# reshape data to long format
rel_dat_long <- rel_by_county_dat |>
  pivot_longer(
    cols = starts_with("Assemblies.of.God"):starts_with("Orthodox"),
    names_to = "affiliation",
    values_to = "proportion"
  )

# apply gsub to convert dots to spaces (for the affiliation column values)
rel_dat_long$affiliation <- gsub(pattern = "\\.", replacement = " ", x = rel_dat_long$affiliation)

# converting column names to snake case
rel_dat_long <- clean_names(rel_dat_long)

# renaming specific columns and adding a new column for year
rel_dat_long <- rel_dat_long |>
  rename(
    total_religious = religious,         
    total_non_religious = non_religious,
    population = pop_2010
  ) |>
  mutate(year = 2010)

# calculate the 'affiliation_population' and 'non_religious_population' columns
rel_dat_complete <- rel_dat_long |>
  mutate(affiliation_population = round(proportion * population),
         non_religious_population = round(total_non_religious * population)) |> # total number of individuals of a specific religious group (or not) in said county
  # change various column data types 
  mutate(affiliation_population = as.integer(affiliation_population),
         year = as.integer(year),
         non_religious_population = as.integer(non_religious_population)
         )

# verify data types 
str(rel_dat_complete)

# rearrange the columns 
rel_dat_complete <- rel_dat_complete |>
  select(county, year, population, total_religious, total_non_religious, non_religious_population, affiliation, proportion, affiliation_population)

# remove the word 'County' from the end of each value within the "county" column 
rel_dat_complete$county <- gsub(" County$", "", rel_dat_complete$county)

#### 3) Explore the cleaned data set with a series of figures (I want to see you exploring the data set) ####

# FIGURE 1
# aggregate population to avoid redundancy
pop_by_county <- rel_dat_complete |>
  group_by(county) |>
  summarise(population = first(population))  # first() used to get one population value per county

# plot population distribution by county using the aggregated data
pop_dist_by_county_plot <- pop_by_county |>  
  ggplot(aes(x = county, y = population)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Population Distribution by County",
       x = "County",
       y = "Population")

print(pop_dist_by_county_plot)

# FIGURE 2 
# summarize total religious and non-religious proportions by county
rel_non_rel_dat <- rel_dat_complete |>
  distinct(county, .keep_all = TRUE) |> # keep one row per county
  summarize(total_religious = sum(total_religious * population),
            total_non_religious = sum(total_non_religious * population),
            total_population = sum(population)) |>
  mutate(religious_pct = total_religious / total_population,
         non_religious_pct = total_non_religious / total_population)

# create a new data frame for the pie chart
rel_non_rel_pie_data <- tibble(
  category = c("Religious", "Non-religious"),
  value = c(rel_non_rel_dat$religious_pct, rel_non_rel_dat$non_religious_pct)
)

# create a pie chart comparing the proportions of religious and non-religious populations across the data set
rel_vs_non_prop_chart <- rel_non_rel_pie_data |>
  ggplot(aes(x = "", y = value, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Proportion of Religious vs. Non-Religious Population across Utah",
       fill = "Population Type") +
  theme_void() +
  theme(axis.text.x = element_blank()) +
  # add percentages to the pie chart
  geom_text(aes(label = paste0(round(value * 100, 1), "%")),
            position = position_stack(vjust = 0.5), color = "white", size = 6)

print(rel_vs_non_prop_chart)

# FIGURE 3
# subset and aggregate that data for only the affiliation population and county information
affil_county_pop_dat <- rel_dat_complete |>
  distinct(county, affiliation, .keep_all = TRUE) |>  # remove redundant rows 
  group_by(county, affiliation) |>  # group by county and affiliation 
  summarize(affiliation_population = sum(affiliation_population), .groups = "drop")  # sum population 

# made my own palete of colors because existing packages like RCBrewer didn't offer enough for how many categories I have
custom_colors <- c(
  "Assemblies of God" = "#E41A1C",   # bright red
  "Buddhism Mahayana" = "#377EB8",   # bright blue
  "Catholic" = "#4DAF4A",            # bright green
  "Episcopal Church" = "#FF7F00",    # bright orange
  "Evangelical" = "#984EA3",         # purple
  "Greek Orthodox" = "#FFFF33",      # bright yellow
  "LDS" = "#A65628",                 # brown
  "Muslim" = "#00CED1",              # teal
  "Non Denominational" = "#999999",  # gray
  "Orthodox" = "#A6CEE3",            # light blue
  "Pentecostal Church of God" = "#B2DF8A",   # light green
  "Southern Baptist Convention" = "#FB9A99", # light red
  "United Methodist Church" = "#FDBF6F"      # light orange
)

# create a stacked bar plot demonstrating the affiliation populations by county 
affil_county_pop_plot <- affil_county_pop_dat |>
  ggplot(aes(x = county, y = affiliation_population, fill = affiliation)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors) +  # Use the custom colors
  labs(title = "Affiliation Populations Across Counties",
       x = "County",
       y = "Affiliation Population",
       fill = "Affiliation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(affil_county_pop_plot)

#### 4) Does population of a county correlate with the proportion of any specific religious group in that county? ####
# correlation between population and the proportion of each religious group
correlations <- rel_dat_complete |>
  group_by(affiliation) |>
  summarize(correlation = cor(population, proportion, use = "complete.obs"))

View(correlations)

# plot the relationship between county population and the proportion of a religious group
correlation_plot <- rel_dat_complete |>
  ggplot(aes(x = population, y = proportion, color = affiliation)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = custom_colors) + # using custom colors from above 
  labs(title = "Proportion of Religious Affiliation vs. County Population",
       x = "County Population",
       y = "Proportion of Affiliation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(correlation_plot)

# There are trends we should address: most of the counties in Utah have a relatively small population. It is within these smaller populations that we find the most variety of religious affiliation proportion (LDS still is the majority in every county).
# However, as those populations do grow, they tend to stay fairly constant in their variations of religious affiliation. It is difficult to see on the plot, but the largest county (Salt Lake), has many different but fairly equal amount of individuals falling under non-LDS affiliation (dots are overlapping and difficult to see).
# Those proportions do stay fairly equal regardless of population size. LDS takes up the majority of religious affiliation.
# The correlation indices calculated above demonstrate a clearer picture. The strongest positive correlation between population size and affiliation is Muslim (0.76) suggesting that the higher the county population, the more Muslims there tend to be.
# Assemblies of God (0.25), Orthodox (0.27), Pentecostal Church of God (0.28), and Greek Orthodox (0.22) all have moderately positive correlations.
# Episcopal Church (-0.06), Southern Baptist Convention (-0.07), and LDS (-0.003) all have very weak correlation indices, indicating little to no correlation between population size and affiliation.
# Non Denominational (-0.0099) and Evangelical (0.04) are both close to zero implying that these are quite stable regardless of population size.

#### 5) Does proportion of any specific religion in a given county correlate with the proportion of non-religious people? ####
# plot the relationship between the proportion of each religious group and non-religious proportion
religion_vs_nonreligion_plot <- rel_dat_complete |>
  ggplot(aes(x = total_non_religious, y = proportion, color = affiliation)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = custom_colors) +  # using custom colors from above 
  labs(title = "Relationship Between Religious Group Proportion and Non-Religious Proportion by County",
       x = "Proportion of Non-Religious Population",
       y = "Proportion of Religious Affiliation",
       color = "Affiliation") +
  theme_minimal()

print(religion_vs_nonreligion_plot)

# Generally speaking, as the proportion of non-religious individuals increases in the population of any given county, the religious proportion most negatively impacted is the LDS populace.
# There is a negative relationship demonstrating that the more non-religious people there are, the less LDS people there are. This is visually clear in the plot as there is a clear regression occurring among the LDS populace.
# Important note: the proportion of other religions (quite small compared to LDS) seems to remain constant regardless of the proportion of non-religious people. This indicates that the most affected group is LDS. 
