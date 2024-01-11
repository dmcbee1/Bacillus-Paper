# Load the dplyr package
library(dplyr)
library(ggplot2)
library(openxlsx)
library(patchwork)

# Importing the dataset without headers and skipping the first row
data <- read.xlsx("/Users/dmcbee/Library/CloudStorage/OneDrive-UniversityofTennessee/Dillon/1 Current Projects/1 Bacterial IPPDMAPP Paper/LCMS/MassHunter_Post_Code_Workup/DPM-F-63_Hex-Piv-Combined-Cleavage.xlsx")

# Add a new column for grouping
data <- data %>%
  mutate(Group = case_when(
    mz %in% c(625.1973, 653.2286) ~ 'A',
    mz %in% c(449.1136, 435.0979) ~ 'B',
    # Add more conditions here as needed
    TRUE ~ 'Other'  # This will assign 'Other' to any mz value not specified above
  ))%>%
  filter(Group %in% c('A', 'B'))
data_summary <- data %>%
  filter(Type == 'Lys')%>%
  group_by(Time, Group) %>%
  summarize(mean_area = mean(area, na.rm = TRUE),
            se_area = sd(area, na.rm = TRUE) / sqrt(n())) %>%
  ungroup()

lysate_only <- data%>%
  filter(Type == 'Lys')

buffer_only <- data %>%
  filter(Type == 'Buffer')

Hex_Only <- data %>%
  filter(Sidechain == 'Hex')

Piv_only <- data %>%
  filter(Sidechain == 'Piv')

barney <- c("#662D91", "#00A651")

# Filter data for each facet
data_facet1 <- lysate_only %>% filter(Group == 'A')
data_facet2 <- lysate_only %>% filter(Group == 'B')

# Create separate plots
plot_facet1 <- ggplot(data_facet1, aes(x = Time, y = area, group = Sidechain, color = Sidechain)) +
  geom_point() + 
  geom_smooth(method = "loess", se = FALSE, span=100) +
  labs(y = "Area") +
  scale_color_manual(values = barney)+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none")+  # y-axis title
  theme_classic() +
  scale_x_continuous(limits = c(0, 48), breaks = seq(0, 48, by = 12))+
  scale_y_continuous(limits = c(0, 20000000))

plot_facet2 <- ggplot(data_facet2, aes(x = Time, y = area, group = Sidechain, color = Sidechain)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, span=100) +
  labs(y = "Area") +
  scale_color_manual(values = barney)+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none")+  # y-axis title
  theme_classic() +
  scale_x_continuous(limits = c(0, 48), breaks = seq(0, 48, by = 12))+
  scale_y_continuous(limits = c(0, 20000000))

# Combine plots
combined_plot <- plot_facet1 / plot_facet2

# Print the combined plot
combined_plot

# Define the file path for the PDF
pdf_file_path <- "/Users/dmcbee/Library/CloudStorage/OneDrive-UniversityofTennessee/Dillon/1 Current Projects/1 Bacterial IPPDMAPP Paper/LCMS/MassHunter_Post_Code_Workup/"  # Replace with your desired file path
csv_name <- "Cleavage_Graph.pdf"
# Export the combined plot as a PDF
ggsave(paste0(pdf_file_path, csv_name), combined_plot, device = "pdf", width = 3.5, height = 3)


