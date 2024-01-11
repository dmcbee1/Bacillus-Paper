#####Inital Data Workup and Libary Loading#####

##Load in Libararies
library(readxl)
library(tidyverse)
library(broom)
library(rstatix)
library(ggpubr)
library(openxlsx)
library(viridis)

##Enter in working directory and Dataset location
dataset1 <-'PathNameOfCSVhere.xlsx'
sheet_name <- 'Plate 1 - Sheet1'
names1 <- 'ExcelFileThatContainsTheNameOfExpementalConditions.xlsx'
new_folder_name <- 'NameofFolderToSaveDataTo'
base_path <- "PathOfTheNewFolder"

# Specify the number of experimental conditions
num_conditions <- 9 #Update this number based on your experiment

# Create the folder name with the current date
folder_name <- paste0(base_path, new_folder_name, Sys.Date())
# Create the new directory
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}

# Set the new directory as the working directory
setwd(folder_name)

#Copy Raw data into new folder
file.copy(from = dataset1, to = folder_name)
file.copy(from = names1, to = folder_name)

#Import Data
treatment1 <- read_excel(dataset1,
                         sheet = sheet_name)%>%
  mutate(Time = seq(0, (n() - 1) * 1/30, 1/30))%>%
  select(-'T° Read 2:400')%>%
  drop_na()

# Calculate the number of columns to keep (3 columns per condition + 1 for the Time column)
num_columns_to_keep <- num_conditions * 3 + 1

# Select the necessary columns (Time column + the first 'num_columns_to_keep - 1' columns of data)
treatment1 <- treatment1 %>%
  select(1:num_columns_to_keep) %>%
  drop_na()

# Replace 'OVRFLW' with NA
treatment1[treatment1 == 'OVRFLW'] <- NA

# Normalization function: Subtracts the first measurement from all values in a vector
normalize_curve <- function(data_vector) {
  # Ensure that the data vector is numeric
  data_vector <- as.numeric(data_vector)
  
  # Use the first non-NA value for normalization
  first_val <- data_vector[!is.na(data_vector)][1]
  
  # If the entire vector is NA, return it as is
  if (is.na(first_val)) {
    return(data_vector)
  }
  
  # Normalize and replace negative values with 0
  normalized_data <- data_vector - first_val
  return(pmax(normalized_data, 0, na.rm = TRUE))
}

# Apply normalization to each growth curve column
for (i in 2:ncol(treatment1)) {
  treatment1[[i]] <- normalize_curve(treatment1[[i]])
}

# Determine the number of groups (assuming first column is 'Time' and the data are in triplicates)
num_groups <- (ncol(treatment1) - 1) / 3

# Generate custom group names dynamically
letters_part <- rep(LETTERS[1:ceiling(num_groups / 4)], each = 4)
numbers_part <- rep(1:4, times = ceiling(num_groups / 4))
custom_group_names <- paste0(letters_part[1:num_groups], numbers_part[1:num_groups])

new_labels_df <- read_excel(names1)
new_labels <- setNames(new_labels_df[[1]], custom_group_names)
custom_labeller <- setNames(as.character(new_labels_df[[1]]), custom_group_names)

# Create an empty list to store data frames
result_list <- list()

# Initialize a counter
counter <- 1

# Loop over the range of column indices (assuming your data starts at 2nd column)
for (i in seq(2, ncol(treatment1), by = 3)) {
  
  # Generate group name dynamically
  group_name <- colnames(treatment1)[i]
  
  # Extract just the part before the number (e.g., "A" from "A1")
  base_group <- substr(group_name, 1, 1)
  
  # Generate a new data frame with the selected columns and new names
  temp_df <- treatment1 %>%
    select(Time, i:(i+2)) %>%
    add_column(group = custom_group_names[counter]) %>%  # Replace 'base_group' with custom group name
    rename(time = "Time",
           w1 = colnames(treatment1)[i],
           w2 = colnames(treatment1)[i+1],
           w3 = colnames(treatment1)[i+2])
  
  # Add the newly created data frame to the list
  result_list[[group_name]] <- temp_df
  
  # Increment the counter
  counter <- counter + 1
}


# Now result_list is a list of data frames, each processed as in your original code

# Combine all data frames into one
combined_df <- dplyr::bind_rows(result_list)


# Reshape to long format
long_df <- tidyr::pivot_longer(
  combined_df,
  c(w1, w2, w3),
  names_to = "replicate",
  values_to = "value")

your_color_values <- c('#1f77b4',
                      '#ff7f0e',
                      '#2ca02c',
                      '#d62728',
                      '#9467bd',
                      '#8c564b',
                      '#e377c2',
                      '#7f7f7f',
                      '#bcbd22')

# Calculate central point (e.g., median) for each group at each time
central_points <- long_df %>%
  group_by(group, time) %>%
  summarise(central_value = median(value))


ggplot(long_df, aes(x = time, y = value, color = group)) +
  geom_point(size = 0.1, alpha = 0.6) +
  geom_smooth(data = central_points, aes(y = central_value, color = group), method = "loess", se = FALSE, size = 0.5, span = 1) +  # Smoothing line
  labs(x = "Time (h)", y = "Absorbance 400 nm", color = "Group") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 1), breaks = c(0.0, 0.5, 1.0)) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 7),  # Reduce text size
        legend.key.size = unit(0.3, "cm"),  # Reduce key size
        legend.spacing.x = unit(0.1, "cm"),  # Adjust horizontal spacing
        legend.spacing.y = unit(0.2, "cm")) +  # Adjust vertical spacing
  scale_color_manual(values = your_color_values, labels = custom_labeller)  # Custom color scale and labels


ggsave(paste0("PNP_OD500_Combined_", new_folder_name, ".png"), width=3, height = 2.5)
ggsave(paste0("PNP_OD500_Combined_", new_folder_name, ".pdf"), width=3, height = 2.5)


## Facuet Graph
ggplot(long_df, aes(x = time, y = value, color = group)) +
  geom_point(size = 1) +
  geom_smooth(data = central_points, aes(y = central_value), method = "gam", se = FALSE, size = 1) +  # Smoothing line
  labs(x = "Time (h)", y = "OD 400 nm", color = "Group") +
  facet_wrap(~ group, labeller = as_labeller(custom_labeller)) +  # updated labeller
  theme_classic() +
  scale_y_continuous(limits = c(0, 1), breaks = c(0.0, 0.5, 1.0)) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none")  # y-axis title

ggsave(paste0("PNP_OD500_Facet_", new_folder_name, ".png"), width=10, height = 7)
ggsave(paste0("PNP_OD500_Facet_", new_folder_name, ".pdf"), width=10, height = 7)

