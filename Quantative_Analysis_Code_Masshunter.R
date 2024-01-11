# Load the dplyr package
library(dplyr)
library(ggplot2)
library(openxlsx)
# Importing the dataset without headers and skipping the first row
data <- read.csv("/Users/dmcbee/Library/CloudStorage/OneDrive-UniversityofTennessee/Dillon/1 Current Projects/1 Bacterial IPPDMAPP Paper/LCMS/DPM_Subtilis_Piv_Cleavage.csv", header = FALSE, skip = 1, stringsAsFactors = FALSE)

# Adjust the number of names based on the number of columns in your data frames
new_column_names <- c("Notebook Page", "Bacteria", "Replicate", "Sidechain",
                      "Type", "Time", "Date and Time", "mz", "rt","area", "height")

#Export path for the csv File
path <- '/Users/dmcbee/Library/CloudStorage/OneDrive-UniversityofTennessee/Dillon/1 Current Projects/1 Bacterial IPPDMAPP Paper/LCMS/MassHunter_Post_Code_Workup/'
csv_name <- 'DPM-F-63_Piv-Cleavage.xlsx'

# Setting the second row as headers
colnames(data) <- as.character(unlist(data[1,]))
data <- data[-1,]

# Checking the first few rows of the data to confirm the headers
head(data)

# Removing the first two columns if they contain '!'
data <- data[, -c(1,2)]

# Checking the data after modifications
head(data)

# Initialize an empty list to store data frames
list_of_dfs <- list()

# Calculate the total number of columns available for sets of four (excluding the first three columns)
total_sets_columns = ncol(data) - 3

# Calculate the number of complete sets of four columns
num_sets <- floor(total_sets_columns / 4)

# Loop through each set of four columns, starting from the fourth column
for (i in 1:num_sets) {
  # Calculate the column indices for the current set
  cols <- (4:7) + (i - 1) * 4
  
  # Select the first three columns and the current set of four columns
  new_df <- data[, c(1:3, cols)]
  
  # Add the new data frame to the list
  list_of_dfs[[i]] <- new_df
}



# Number of expected splits (change this number based on your data)
num_splits <- 6  # Example: 6 splits resulting in 6 new columns

# Loop through each data frame in the list
for (i in 1:length(list_of_dfs)) {
  # Split the first column on underscore
  split_data <- strsplit(as.character(list_of_dfs[[i]][,1]), "_")
  
  # Dynamically create new columns based on the number of splits
  for (j in 1:num_splits) {
    # Create a new column for each part of the split
    # The ifelse statement ensures that the code does not break if the number of parts is less than expected
    list_of_dfs[[i]][, paste("Part", j, sep = "")] <- sapply(split_data, function(x) ifelse(length(x) >= j, x[j], NA))
  }
  
  # Create a vector of new column names
  new_col_names <- paste("Part", 1:num_splits, sep = "")
  
  # Rearrange the columns to move the new columns to the front
  list_of_dfs[[i]] <- list_of_dfs[[i]][, c(new_col_names, setdiff(names(list_of_dfs[[i]]), new_col_names))]
}

#Remove original dataset column
# Loop through each data frame in the list
for (i in 1:length(list_of_dfs)) {
  # Remove the "Data File" column
  list_of_dfs[[i]]$`Data File` <- NULL
  # Remove the "Type" column
  list_of_dfs[[i]]$`Type` <- NULL
}

# Now, each data frame in 'list_of_dfs' no longer has the "Data File" column


# Loop through each data frame in the list
for (i in 1:length(list_of_dfs)) {
    colnames(list_of_dfs[[i]]) <- new_column_names
}

# Loop through each data frame in the list
for (i in 1:length(list_of_dfs)) {
  # Check if the "Time" column exists in the data frame
  if ("Time" %in% names(list_of_dfs[[i]])) {
    # Replace "h.d" with a decimal point in the "Time" column
    list_of_dfs[[i]]$Time <- gsub("h\\.d", "", list_of_dfs[[i]]$Time)
  } else {
    warning(paste("Time column not found in data frame", i))
  }
}

# Combine the list of data frames into one data frame
combined_data <- bind_rows(list_of_dfs)

# Convert the 'Time' column to numeric if it's not already
combined_data$Time <- as.numeric(as.character(combined_data$Time))

# Convert the 'area' column to numeric
combined_data$area <- as.numeric(as.character(combined_data$area))

# Convert the 'height' column to numeric
combined_data$height <- as.numeric(as.character(combined_data$height))

# Correct file path concatenation
full_path <- paste0(path, csv_name)

# Write the combined_data data frame to an Excel file
write.xlsx(combined_data, file = full_path)


ggplot(combined_data%>% filter(Type == 'Buffer'), aes(x = Time, y = area, group = mz, color = mz, shape = Type)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, span=100) + 
  labs(title = "Comparison of m/z Values Over Time",
       x = "Time",
       y = "Area")+
  scale_x_discrete(limits = c(0, 24, 48))+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none")+  # y-axis title
  theme_classic()
