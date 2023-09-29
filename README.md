# LeTourneux_et_al_code_data_private_for_review
This repository contains all code needed to reproduce analyses of LeTourneux et al., manuscript entitled "Evidence for seasonal compensation of hunting mortalities in a long-lived migratory bird.". Data for all scripts and to reproduce all figures is provided in the repository. The compiled capture-recapture dataset to reproduce the E-surge analyses will be made available upon acceptation of the paper for publication.

All capture-mark-recapture analyses were conducted in the software E-SURGE and all figures and some analyses to generate the figures were made with R. The script to generate figures from E-SURGE model results are present in R scripts. There is one script for the figures in the main text, and then each appendix has its own stand alone script for the figures. E-SURGE model output files are named in a self-explanatory way, e.g., "M6_this_study.csv" contains parameter estimates from model M6 of this study in a csv2 file. Parameter estimates are on the [0-1] scale, and are provided along with their SE and 95% confidence intervals. For files containing betas, betas are provided on the real scale along with their SEs and 95% confidence intervals. Some files also contain model outputs from LeTourneux et al. 2022 J. Appl. Ecol. (doi: 10.1111/1365-2664.14268), for means of comparison.

The code for our most parsimonious E-surge model is provided in the file "E-Surge_code_model_M10.docx". Some basic knowledge of E-Surge is needed to fit the model, however, a detailed explanation is given on how to set the initial values as this step is not intuitive.


Description of data files:

1. harvest_rate_GSG.csv: File saved to csv2 format (sep=';'). 
Harvest rates of adult greater snow geese between 1990 and 2019. 

Column description: 
 - Year: year;
 - HR: harvest rate (adults only), see details in main text for calculation of harvest rate.


2. Model output files I (M6_this_study.csv, M10_this_study.csv, M14_LeTourneux_et_al_2022.csv, M17_LeTourneux_et_al_2022.csv)

Column description: 
 - Parameters: Refers to the different matrices and steps. IS: Initial states, C: 1st transition matrix (collar loss), S: 2nd transition matrix (survival); E: 1st event matrix, E2: 2nd event matrix.
 - From: State of origin (i.e., matrix lines). Varies based on model and matrix. See Appendix 1 in this study and LeTourneux et al. 2022 for details.
 - To: State of arrival (i.e., matrix columns)
 - Time: Occasion
 - Age: Age class, used to model change in collar loss probabilities through time (See LeTourneux et al. 2022 for details)
 - Group: Grouping factor for the data. Not used in this study
 - Step: Matrix step (information already included in 'Parameters')
 - Estimates: Parameter estimates on the [0-1] scale
 - CI-: Lower 95% confidence interval of parameter
 - CI+: Upper 95% confidence interval of parameter
 - SE: Standard error of parameter

