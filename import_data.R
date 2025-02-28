# import global and baro data and write to dir
library(kofdata)
library(tidyverse)
library(stats)
library(scales)
library(zoo)

# import data
baro <- get_time_series("kofbarometer")
baro_changed <- lapply(baro, "*", 3)
baro <- ts(baro, start = c(1991, 1), frequency = 12)
baro_changed <- ts(baro_changed, start = c(1991, 1), frequency = 12)

# create a new folder structure in this repo
# Define the main directory path
new_folder_path <- file.path(getwd(), "data/ch/kof")

# Function to create directories
create_sub_folders <- function(x) {
  if (!dir.exists(x)) {
    dir.create(x, recursive = TRUE) # Creates parent directories if they don't exist
    print(paste("Created directory:", x))
  } else {
    print(paste("Directory already exists:", x))
  }

  # Create subdirectories within x
  sub_dirs <- c("baro", "baro2")
  for (sub in sub_dirs) {
    sub_path <- file.path(x, sub)
    if (!dir.exists(sub_path)) {
      dir.create(sub_path)
      print(paste("Created subdirectory:", sub_path))
    } else {
      print(paste("Subdirectory already exists:", sub_path))
    }
  }
}

# Call the function
# create_sub_folders(new_folder_path)

# write data
getwd()
write.csv(baro$kofbarometer, "data/ch/kof/baro/series.csv", row.names = FALSE)
write.csv(baro_changed$kofbarometer, "data/ch/kof/baro2/series.csv", row.names = FALSE)

# v2
# random_change <- function(ts) {
#   num <- floor(length(ts) * 0.2)
#   print(num)
#   rep <- sample.int(1, num, replace = TRUE)
#   print(num)
#   nums_to_modify <- sample(1:ceiling(length(ts) * 0.2), rep)
#   # list_num_to_modify <- sample(1:length(ts),
#   # )
#   # list_to_modify <- list()
# }


# random changes
modify_and_append_ts <- function(ts_values) {
  # determines the amount of data points to change
  num_to_modify <- sample(1:ceiling(length(ts_values) * 0.2), 1)
  # Select random indices to modify
  indices_to_modify <- sample(2:length(ts_values), num_to_modify, replace = FALSE)

  # Modify selected points by ±10% of the previous value
  for (i in indices_to_modify) {
    change <- runif(1, -0.1, 0.1) # Random change between -10% and +10%
    ts_values[i] <- ts_values[i - 1] * (1 + change)
  }

  # Append a new value at the end (10% variation from the last value)
  last_value <- ts_values[length(ts_values)]
  new_value <- last_value * (1 + runif(1, -0.1, 0.1))
  new_ts <- ts(c(ts_values, new_value), start = start(ts_values), frequency = frequency(ts_values))

  return(new_ts)
}

new_kofbaro <- modify_and_append_ts(baro$kofbarometer)
new_kofbaro2 <- modify_and_append_ts(baro_changed$kofbarometer)

write.csv(new_kofbaro, "data/ch/kof/baro/series.csv", row.names = FALSE)
write.csv(new_kofbaro2, "data/ch/kof/baro2/series.csv", row.names = FALSE)
