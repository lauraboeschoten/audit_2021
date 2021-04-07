# CBS_audit_2020
This github repository contains all the R code that has been used to perform the simulation study found in the paper 
'efficient audit sample selection'. The complete procedure of the simulation study can be walked through by following the numbering
of the scripts. 

1. The scripts '1_data_generation_....R' contain the code that has been used to generate the datasets under varying conditions tested 
in the simulation study. 

2. The scripts '2_apply_proceudre_....R' contain the code that has been used to apply the audit sample procedure to each of the conditions.
These scripts also call the subscripts '2a_....R' - '2h_....R' which contain functions needed to apply the audit sample procedure or to
calculate the output that has been reported in the paper. 

3. The scripts '3_evaluate_outut_....R' generate tables with results from the simulation output.

4. The scripts '4_plot_output_....R' generate plots with results from the simulation output.

5. The scripts '5_plot_....R' generate the boxplots that have been included in the manuscript

The folder 'reproducible example' contains a simulated dataset and the functions needed to apply the audit sample selection procedure 
on this dataset. This dataset can be used as an example of how to prepare your data and these functions can be used to apply the procedure
to your own dataset. 
