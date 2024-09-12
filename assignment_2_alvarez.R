# Assignment 2 
#### Step 4 ####

# creates an object that contains all files found in the Data/ directory labeled .csv
csv_files <- list.files(path = "Data/", pattern = "\\.csv$", full.names = TRUE)

#### Step 5 ####

# count the number of total files in the csv_files object 
length(csv_files)

#### Step 6 ####

# read the wingspan_vs_mass.csv file into the df object 
df <- read.csv("Data/wingspan_vs_mass.csv")

#### Step 7 ####

# inspect the first 5 lines of the wingspan_vs_mass.csv file 
head(df, 5)

#### Step 8 ####

# creates an object of files (recursively) in the Data/ directory that begin with lowercase b 
b_files <- list.files(path = "Data/", pattern = "^b", full.names = TRUE, recursive = TRUE)

#### Step 9 ####

# loops through b_files and prints out the first line of each individual file within the object 
for (file in b_files) {
  first_line <- readLines(file, n = 1)
  print(first_line)
}

#### Step 10 ####

# the following code uses the same function as Step 9 but uses an updated csv_files object instead
# loops through recursively by including the 'recursive = TRUE' argument to the object declaration
csv_files_2 <- list.files(path = "Data/", pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

for (file in csv_files_2) {
  first_line <- readLines(file, n = 1)
  print(first_line)
}
