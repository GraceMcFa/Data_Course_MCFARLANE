## Assignment 2 ##
# 4. Write a command that lists all of the .csv files found in the Data/ directory and stores that list in an object called "csv_files" 
csv_files <- list.files(path = "Data/", pattern = "\\.csv$", full.names = TRUE)
print(csv_files)

# 5. Find how many files match that discription using the lenghth() function
num_csv_files <- length(csv_files)
print(num_csv_files)

# 6. Open the wingspan_vs_mass.csv file and store the contents as an R object named "df" using the read.csv() function 
df <- read.csv("Data/wingspan_vs_mass.csv")
head(df)

# 7. Inspect the first 5 lines of this data set using the head() function 
head(df, 5)

# 8. Find any files (recursively) in the Data/ directory that begin with the letter "b" (lowercase)
b_files <- list.files(path = "Data/", pattern = "^b", full.names = TRUE, recursive = TRUE)
print(b_files)

# 9. Write a command that displays the first line of each of those "b" files (this is tricky... use a for-loop)
# List all files starting with "b"
b_files <- list.files(path = "Data/", pattern = "^b", full.names = TRUE, recursive = TRUE)

# Loop through each file and display the first line
for (file in b_files) {
  # Print the file name
  cat("File:", file, "\n")
  
  # Read and display the first line
  first_line <- readLines(file, n = 1)
  cat("First line:", first_line, "\n\n")
}

# 10. Do the same thing for all files that end in ".csv" 
# List all .csv files in the directory (recursively)
csv_files <- list.files(path = "Data/", pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

# Loop through each .csv file and display its first line
for (file in csv_files) {
  # Print the file name
  cat("File:", file, "\n")
  
  # Read and display the first line
  first_line <- readLines(file, n = 1)
  cat("First line:", first_line, "\n\n")
}


