# Bacillus Paper
 
Bradford_Assay_Code <- Generates standard curve and determines protein concentration of unknowns. Inputs must be added directly to code.

Data Visualization for Growth Curves <- This code handles data exported from the Gen5 program used with the Cytation 1 (Biotek) cell imager and plate reader:

  Data Input:
  We begin by importing an Excel sheet containing the data.
  Growth Curves Creation:
  Growth curves will be generated based on triplicate measurements.
  
  Faceted Graphs:
  For each set of triplicates, faceted graphs are produced.
  These graphs compare each growth curve to the reference wells A4-A6.
  
  Area Under the Curve (AUC) Calculation:
  For each growth curve, the AUC is determined.
  The average and standard deviation of the AUC for the triplicates are then computed.
  
  Histogram Generation:
  A histogram of the AUC values is produced.
  Error bars on the histogram represent the standard deviation (both + and -).
  
  ANOVA Analysis:
  An Analysis of Variance (ANOVA) is performed based on the AUC values.
  Results are exported to an Excel sheet.
  Additionally, a graphical representation of the ANOVA results is provided.

  Data must be in the following format in excel to work in the code (5 lines of excel       document shown):
  600							
  Time	T° 600	A1	A2	A3	A4	A5	A6.... and so on for as many triplicates as avaliable
  0:08:45	37	0.087	0.09	0.089	0.102	0.097	0.103
  0:18:45	37	0.087	0.093	0.09	0.102	0.099	0.104
  0:28:45	37	0.087	0.088	0.089	0.106	0.098	0.101

Quantative_Analysis_Code_Masshunter <- This script is designed to process, analyze, and visualize mass spectrometry data exported from MassHunter Quantative Analysis

  Overview:
  Loading Libraries:
  The script utilizes the tidyr, dplyr, and ggplot2 libraries for data manipulation and       visualization.
  
  Data Import:
  The script reads data from a CSV file. Users must provide the path to the CSV file in the   code.
  
  Data Cleaning:
  Rows with missing pH values are filtered out to ensure data consistency.
  
  Data Organization:
  The dataset is split into separate data frames based on masses.
  Columns of each data frame are renamed for consistency.
  All separated data frames are then combined into one unified dataset.

  Visualization:
  Two types of plots are generated:
  A plot with a logarithmic scale for Peak Area.
  A plot without a logarithmic scale for Peak Area.

  Custom Functions:
  log_plot: Generates plots on a logarithmic scale for specified m/z values.
  linear_plot: Generates plots on a linear scale for specified m/z values.

  Unique m/z Values:
  A list of unique m/z values present in the data is created.
  
  Data must be in the following format in excel to work in the code (5 lines of excel         document shown):
  (FIRST ROW IS INGORED, second row and onward shown below) 		
  pH	Type	Time	Product Ion	RT	Area	Height	Product Ion	RT	Area	Height...
  6.5	Piv	12	287.069				435.0979	4.029	6798	2444
  7.5	Piv	12	287.069				435.0979	4.029	22043	7916
  6.5	Piv	12	287.069				435.0979	4.022	7291	2402