# Load necessary libraries for data manipulation and visualization
library(tidyr)
library(dplyr)
library(ggplot2)

# Read the data from the CSV file
data <- read.csv('', header=TRUE, skip=1) # Provide the path to the CSV file inside the quotes

# Filter out rows with missing pH values
data <- data %>%
  filter(!is.na(pH))

# Split the dataset into separate data frames based on masses
list_of_seperated_masses <- lapply(seq(4, ncol(data), by=4), function(start_col) {
  end_col <- min(start_col + 4 - 1, ncol(data))
  data[, c(1:3, start_col:end_col)]
})

# Rename columns of each data frame for consistency
new_names <- c("Sidechain","Group", 'Time', "mz", "RT", "Area", 'Height')
list_of_renamed_masses <- lapply(list_of_seperated_masses, function(df) {
  setNames(df, new_names)
})

# Combine all separated data frames into one and filter for specified group
combined_data <- bind_rows(list_of_renamed_masses, .id = "Comb")%>%
  filter(Group == '') #Insert name of specificed group or remove this line of code

# Plot for all masses with log scale for Peak Area
ggplot(combined_data, aes(x=Time, y=log(Area), color=Sidechain)) +
  geom_point(size=2) +
  geom_smooth(method= 'lm',se = FALSE, aes(group = Sidechain),size=0.5) +
  facet_wrap(~ mz) +
  labs(x="Time (h)", 
       y="log Peak Area", 
       color="Sidechain") +
  theme_classic() 

# Plot for all masses without log scale for Peak Area
ggplot(combined_data, aes(x=Time, y=Area, group=sidechain, color=sidechain)) +
  geom_point() +
  facet_wrap(~ mz) +
  labs(title="Peak Area Over Time",
       x="Time (h)", 
       y="Peak Area", 
       color="pH") +
  theme_classic()

# Function to generate log scale plots for specified m/z values
log_plot <- function(data_frame, mz_values) {
  filtered_data <- data_frame %>% filter(mz %in% mz_values)
  plot <- ggplot(filtered_data, aes(x=Time, y=log(Area), group=pH, color=pH)) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, aes(group = pH)) +
    facet_wrap(~ mz) +
    labs(title=paste("Peak Area Over Time for m/z Values:", paste(mz_values, collapse=", ")),
         x="Time (h)", 
         y="Peak Area", 
         color="pH") +
    theme_classic()
  return(plot)
}

# Function to generate linear scale plots for specified m/z values
linear_plot <- function(data_frame, mz_values) {
  filtered_data <- data_frame %>% filter(mz %in% mz_values)
  plot <- ggplot(filtered_data, aes(x=Time, y=Area, group=pH, color=pH)) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, aes(group = pH)) +
    facet_wrap(~ mz) +
    labs(title=paste("Peak Area Over Time for m/z Values:", paste(mz_values, collapse=", ")),
         x="Time (h)", 
         y="Peak Area", 
         color="pH") +
    theme_classic()
  return(plot)
}

# Get a list of unique m/z values in the data
unique_mz_values <- unique(combined_data$mz)
unique_mz_values

# Generate a log scale plot for m/z value of 4
log_plot(combined_data, 4)

