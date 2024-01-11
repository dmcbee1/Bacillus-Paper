# Exogenous cellular delivery of isopentenyl pyrophosphate and dimethylallyl pyrophosphate to Bacillus subtilis
 
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

Data must be in the following format in excel to work in the code (5 lines of excel document shown):
  600							
  Time	T° 600	A1	A2	A3	A4	A5	A6.... and so on for as many triplicates as available
  0:08:45	37	0.087	0.09	0.089	0.102	0.097	0.103
  0:18:45	37	0.087	0.093	0.09	0.102	0.099	0.104
  0:28:45	37	0.087	0.088	0.089	0.106	0.098	0.101

PNP_nitrophenol_Assay <- Processes an Excel data file containing absorbance values, processes   triplicate readings, and then generates growth curve plots with options for visualizing the  data in different ways.

Similar to the script 'Data Visualization for Growth Curves'
 
Quant_Analysis_Code_Masshunter-Data-Rearrangement <- This script is designed to process, analyze, and visualize mass spectrometry data exported from MassHunter Quantitative Analysis. *Note this code is dependent on how the mass spectrometry runs are saved and will need to be adjusted based on your specific needs.

Data Import: The script starts by importing data from a CSV file. Modify "PathNameOfCSVhere.csv" to the path of your CSV file. The data is imported without headers   and skips the first row.

Column Renaming: The script sets new column names. Adjust new_column_names as per your dataset's requirements.

Data Cleaning: It includes removing unwanted columns and setting custom headers.

Data Splitting and Rearranging: The script splits data based on underscores in specific columns, rearranges the data, and removes unnecessary columns.

Data Combination and Conversion: All the split and cleaned data frames are combined into one. Specific columns ('Time', 'area', and 'height') are converted to       numeric types.

Export: The final combined data frame is exported as an Excel file. Set the path and csv_name variables to your desired output directory and file name.

Mass_Hunter_Quant_Graph <- This script will take the output from Quant_Analysis_Code_Masshunter-Data-Rearrangement and provide comparative graphs.

Data Import: Import data from an Excel file. Replace "Name of export from 'Quantative_Analysis_Code_Masshunter.xlsx" with your file's name.

Data Grouping and Filtering: The script groups data based on m/z values into categories 'A', 'B', and 'Other'. It then filters data for specific types ('Lys', 'Buffer', etc.) and sidechains ('Hex', 'Piv').

Data Summarization: Calculates mean and standard error (SE) of the 'area' for each 'Time' and 'Group' combination.

Visualization Preparation: Prepares individual plots for different data subsets (e.g., lysate_only, buffer_only) with specific aesthetic settings.

Plot Combination: Uses patchwork to combine individual plots into a single figure.

Exporting the Combined Plot: The script exports the combined plot as a PDF. Set pdf_file_path and csv_name to your desired output directory and file name.
