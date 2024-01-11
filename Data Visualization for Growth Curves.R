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
dataset1 <-'/Users/dmcbee/Library/CloudStorage/OneDrive-UniversityofTennessee/Dillon/1 Current Projects/1 Bacterial IPPDMAPP Paper/Growth Curves/DPM-L-1_Hex-Alk_subtilis_Tox-Test_12920203.xls'
sheet_name <- 'Plate 1 - Sheet1'
names1 <- '/Users/dmcbee/Library/CloudStorage/OneDrive-UniversityofTennessee/Dillon/1 Current Projects/1 Bacterial IPPDMAPP Paper/Growth Curves/Name_Template.xlsx'
new_folder_name <- 'DPM-L-1_Hex-Alk_subtilis_Tox-Test_'
base_path <- "/Users/dmcbee/Library/CloudStorage/OneDrive-UniversityofTennessee/Dillon/1 Current Projects/1 Bacterial IPPDMAPP Paper/Growth Curves/Processed_Growth_Curves/"

# Specify the number of experimental conditions
num_conditions <- 7  # Update this number based on your experiment

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
  sheet = sheet_name, skip = 1)%>%
  mutate(Time = seq(0, (n() - 1) * 1/6, 1/6))%>%
  select(-'T° 600')%>%
  drop_na()

# Calculate the number of columns to keep (3 columns per condition + 1 for the Time column)
num_columns_to_keep <- num_conditions * 3 + 1

# Select the necessary columns (Time column + the first 'num_columns_to_keep - 1' columns of data)
treatment1 <- treatment1 %>%
  select(1:num_columns_to_keep) %>%
  drop_na()

# Normalization function: Subtracts the first measurement from all values in a vector
normalize_curve <- function(data_vector) {
  normalized_data <- data_vector - data_vector[1]
  return(pmax(normalized_data, 0))  # Replace negative values with 0
}


# Apply normalization to each growth curve column Assuming your growth curve data starts from the 2nd column
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

# Filter rows where time <= 10.00
filtered_df <- dplyr::filter(combined_df, time <= 10.00)

# Reshape to long format
long_df <- tidyr::pivot_longer(
  filtered_df,
  c(w1, w2, w3),
  names_to = "replicate",
  values_to = "value"
)


# Calculate central point (e.g., median) for each group at each time
central_points <- long_df %>%
  group_by(group, time) %>%
  summarise(central_value = median(value))

#####Plotting Growth Curves#####

# Plot with facet_wrap, classic theme, smaller points, and smoothed line

ggplot(long_df, aes(x = time, y = value, color = group)) +
  geom_point(size = 1) +
  geom_smooth(data = central_points, aes(y = central_value), method = "gam", se = FALSE, size = 1) +  # Smoothing line
  labs(x = "Time (h)", y = "OD 600 nm", color = "Group") +
  facet_wrap(~ group, labeller = as_labeller(custom_labeller)) +  # updated labeller
  theme_classic() +
  scale_y_continuous(limits = c(0, 1))+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none")  # y-axis title

ggsave(paste0("Growth_Curves_Facet_", new_folder_name, ".tiff"), width=10, height = 7)
ggsave(paste0("Growth_Curves_Facet_", new_folder_name, ".pdf"), width=10, height = 7)

# Create a subfolder for the graphs
graphs_folder <- file.path(folder_name, "Growth_Curve_Graphs")
if (!dir.exists(graphs_folder)) {
  dir.create(graphs_folder)
}

# Loop through each entry in custom_labeller, create and save the plot
for (group_name in names(custom_labeller)) {
  condition_label <- custom_labeller[group_name]
  
  # Filter the data for the current group
  condition_data <- filter(long_df, group == group_name)
  
  # Generate the plot for the current condition
  plot <- ggplot(condition_data, aes(x = time, y = value)) +
    geom_point(size = 1) +
    geom_smooth(method = "gam", se = FALSE, size = 1) +  # Smoothing line
    labs(title = paste("Growth Curve for", condition_label), x = "Time (h)", y = "OD 600 nm") +
    theme_classic() +
    scale_y_continuous(limits = c(0, 1)) +
    theme(axis.text.x = element_text(size = 8, color = "black"),
          axis.text.y = element_text(size = 8, color = "black"),
          axis.title.x = element_text(size = 10, color = "black"),
          axis.title.y = element_text(size = 10, color = "black"),
          plot.title = element_text(size = 8, color = "black"),  # Set the title color to black
          legend.position = "none")
  # Save the plot in the graphs subfolder
  ggsave(filename = paste0(graphs_folder, "/", condition_label, ".pdf"), plot, width = 3, height = 2.5)
}

#####Graph a Filtered Facet Graph
# List of specific treatments to include
include_treatments <- c("C4", "A1") 
# Update this with the actual group names you want to include


# Filter the dataframe to include only specified treatments
long_df_filtered <- long_df %>% 
  filter(group %in% include_treatments)

# Plot with facet_wrap using the filtered dataframe
ggplot(long_df_filtered, aes(x = time, y = value, color = group)) +
  geom_point(size = 0.1)+
  geom_smooth(data = central_points %>% filter(group %in% include_treatments), 
              aes(y = central_value), method = "gam", se = FALSE, size = 0.5) +
  labs(x = "Time (h)", y = "OD 600 nm", color = "Group") +
  facet_grid(~ group, labeller = as_labeller(custom_labeller)) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 1), breaks = c(0.0, 0.5, 1.0)) +
  scale_x_continuous(limits = c(0, 10), breaks = c(0, 5, 10)) +
  theme(
    axis.text.x = element_text(size = 10, colour = "black"),
    axis.text.y = element_text(size = 10, colour = "black"),
    axis.title.x = element_text(size = 12, colour = "black"),
    axis.title.y = element_text(size = 12, colour = "black"),
    strip.text = element_text(size = 4, colour = "black", margin = margin(1, 1, 1, 1, "pt")),  # Adjust the size and color of facet labels
    strip.background = element_blank(),
    legend.position = "none",
    plot.title = element_text(colour = "black"),
    plot.subtitle = element_text(colour = "black"),
    plot.caption = element_text(colour = "black")
  )



ggsave(paste0("Growth_Curves_Selected_5-1_blank-rescue_", new_folder_name, ".pdf"), width=3, height = 1.5)

#####Direct comparsion to Vehicle####

# Filter for time <= 10
filtered_long_df <- long_df %>%
  filter(time <= 10)

# Extract just the "A2" group data
a2_data <- filtered_long_df %>%
  filter(group == "A2")

# Combine 'A2' data with each other group
combined_list <- lapply(unique(filtered_long_df$group), function(grp) {
  if (grp != "A2") {
    tmp_data <- filtered_long_df %>%
      filter(group == grp)
    combined_df <- bind_rows(a2_data, tmp_data)
  } else {
    combined_df <- a2_data
  }
  combined_df$facet_group <- grp
  return(combined_df)
})

# Bind all the data frames into a single one
final_combined_df <- do.call(rbind, combined_list)

# Custom labeller function
custom_labeller <- as_labeller(new_labels)

# Generate the plot
ggplot(final_combined_df, aes(x = time, y = value, color = group)) +
  geom_point(size = 1) + 
  geom_smooth(method = 'gam', se = FALSE) +
  facet_wrap(~ facet_group, labeller = as_labeller(custom_labeller)) +  # Added the custom labeller here
  scale_color_manual(values = c("A2" = "red", "Other" = "purple")) +
  labs(x = "Time (h)", y = "OD 600 nm", color = "Group") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 1))+
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.position = "none")

ggsave(paste0("Growth_Curves_Comparison_To_Vehicle", new_folder_name, ".tiff"), width=10, height = 7)
ggsave(paste0("Growth_Curves_Comparison_To_Vehicle", new_folder_name, ".pdf"), width=10, height = 7)

###Find Area under the Curve for each Condition####
# Load the required library
library(pracma) 

compute_individual_auc <- function(data) {
  return(pracma::trapz(data$time, data$value))
}

# Calculate AUC for each group and replicate
auc_replicate_data <- long_df %>%
  group_by(group, replicate) %>%
  do(auc = compute_individual_auc(.)) %>%
  ungroup()

# Unlist the auc values
auc_replicate_data$auc <- unlist(auc_replicate_data$auc)

# Calculate mean and standard deviation for each group
auc_summary <- auc_replicate_data %>%
  group_by(group) %>%
  summarise(
    mean_auc = mean(auc),
    sd_auc = sd(auc),
    n = n()
  ) %>%
  mutate(se_auc = sd_auc / sqrt(n), # Standard error
         lower = mean_auc - se_auc,
         upper = mean_auc + se_auc)

# Obtain A2 AUC
a2_mean_auc <- filter(auc_summary, group == "A2")$mean_auc

# Convert mean AUC and SD to percentages of A2
auc_summary <- auc_summary %>%
  mutate(
    perc_mean_auc = (mean_auc / a2_mean_auc) * 100,
    perc_sd_auc = (sd_auc / a2_mean_auc) * 100
  )

# Plot AUC as percentages with error bars
ggplot(auc_summary, aes(x = group, y = perc_mean_auc)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  geom_errorbar(
    aes(ymin = perc_mean_auc - perc_sd_auc, ymax = perc_mean_auc + perc_sd_auc),
    width = 0.3,
    position = position_dodge(width = 0.9)
  ) +
  labs(y = "Area Under the Curve (% of Control)", x = "") +  # Remove x-axis title
  scale_x_discrete(labels = custom_labeller, expand=c(0.075, 0.05)) +
  scale_y_continuous(expand=c(0, 0), limits = c(0, 125)) +
  theme_classic() +  # Apply classic theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3, color = "black"),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.title.y = element_text(size = 8, color = "black"),
        plot.title = element_text(size = 8, color = "black"),
        legend.position = "none")


ggsave(paste0("AUC_as-percent-of-Veh_", new_folder_name, ".png"), width=3, height = 3)
ggsave(paste0("AUC_as-percent-of-Veh_", new_folder_name, ".pdf"), width=3, height = 3)

# Replace group names in auc_summary with descriptive names
auc_summary$group <- new_labels[auc_summary$group]
# Define the file path for the Excel file
excel_file_path <- paste0("AUC_Summary_", new_folder_name, ".xlsx")

# Export the auc_summary data frame to an Excel file
write.xlsx(auc_summary, file = excel_file_path)


####Anova to look for changes between groups#####
# ANOVA
auc_anova <- aov(auc ~ group, data = auc_replicate_data)

anova_table <- summary(auc_anova)
print(anova_table)

# Post hoc test (Tukey HSD)
tukey_result <- TukeyHSD(auc_anova)
print(tukey_result)

# Extract the comparison data
comparisons <- as.data.frame(tukey_result$group)
# Rename the column to remove the space
colnames(comparisons)[colnames(comparisons) == "p adj"] <- "p.adj"
# Add significance
comparisons$significant <- ifelse(comparisons$p.adj < 0.05, "Significant", "Not Significant")

comparisons$group_comparison <- rownames(comparisons)

# Create a custom ordering key for the comparisons
comparisons$order_key <- with(comparisons, 
                              ifelse(grepl("^A1-", group_comparison), 
                                     paste0("1", group_comparison), 
                                     paste0("2", group_comparison)))

ggplot(comparisons, aes(x=reorder(group_comparison, order_key), y=diff)) +
  geom_point(aes(color=significant)) +
  geom_errorbar(aes(ymin=lwr, ymax=upr, color=significant), width=0.2) +
  coord_flip() +  # Makes it horizontal
  labs(title="Tukey's HSD Post-hoc Test",
       x="Group Comparisons",
       y="Difference in Means") +
  theme_minimal() +
  scale_color_manual(values=c("Significant"="red", "Not Significant"="blue")) +
  theme(legend.position="top")
# Save the plot
ggsave(paste0("Tukey_postHoc_", new_folder_name, ".tiff"), width=7, height = 10)
ggsave(paste0("Tukey_postHoc_", new_folder_name, ".pdf"), width=7, height = 10)

# Convert TukeyHSD object to dataframe
tukey_df <- data.frame(tukey_result$group)  # Assuming 'group' is the name of your factor variable in the anova.

# Add a new column with the comparisons
tukey_df$Comparison <- rownames(tukey_df)

# Reorder columns to have "Comparison" column first
tukey_df <- tukey_df[, c(ncol(tukey_df), 1:(ncol(tukey_df)-1))]

# Export to Excel
write.xlsx(tukey_df, "tukey_results.xlsx")

