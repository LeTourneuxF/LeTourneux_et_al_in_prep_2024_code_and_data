# LeTourneux_et_al_in_prep_code_and_data


This repository contains all code needed to reproduce analyses of LeTourneux et al., manuscript entitled "Evidence for seasonal compensation of hunting mortalities in a long-lived migratory bird.". Data for all scripts and to reproduce all figures is provided in the repository. The compiled capture-recapture dataset to reproduce the E-surge analyses will be made available upon acceptation of the paper for publication.

All capture-mark-recapture analyses were conducted in the software E-SURGE and all figures and some analyses to generate the figures were made with R. The script to generate figures from E-SURGE model results are present in R scripts. There is one script for the figures in the main text, and then each appendix has its own stand alone script for the figures. E-SURGE model output files are named in a self-explanatory way, e.g., "M6_this_study.csv" contains parameter estimates from model M6 of this study in a csv2 file. Parameter estimates are on the [0-1] scale, and are provided along with their SE and 95% confidence intervals. For files containing betas, betas are provided on the real scale along with their SEs and 95% confidence intervals. Some files also contain model outputs from LeTourneux et al. 2022 J. Appl. Ecol. (doi: 10.1111/1365-2664.14268), for means of comparison.

The code for our most parsimonious E-surge model is provided in the file "E-Surge_code_model_M10.docx". Some basic knowledge of E-Surge is needed to fit the model, however, a detailed explanation is given on how to set the initial values as this step is not intuitive.


Description of data files:

1. harvest_rate_GSG.csv: File saved to csv2 format (sep=';'). Harvest rates of adult greater snow geese between 1990 and 2019.

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

3. Model output files II (M6_this_study_betas.csv, M5_this_study_betas.csv)
   
   Column description:  

   - Index: Line number
   - Value: beta of parameter on the real scale ]-Inf,+Inf[
   - CI-: Lower 95% confidence interval of beta
   - CI+: Upper 95% confidence interval of beta
   - SE: Standard error of beta
   - beta: Description of beta represented by each line. Mostly used to extract betas for script and figures
	- IS1: Prob. of being ring weakly obs(Index=1); Prob. of being ring highly obs(Index=2); Prob. of being col highly obs summer(Index=3); 
	- IS2: Prob. of being col highly obs during fall
	- IS3: Prob. of being col highly obs during winter
	- IS4: Prob. of being col highly obs during spring
	- perte_col: Prob. of losing collar
	- S_ete: Survival ringed only summer
	- S_fall: Survival ringed only fall
	- S_wint: Survival ringed only winter
	- S_spr: Survival ringed only spring
	- col_ete_d2: Collar effect on survival summer hunting period 2
	- col_ete_d3: Collar effect on survival summer hunting period 3
	- col_fall_d2: Collar effect on survival fall hunting period 2
	- col_fall_d3: Collar effect on survival fall hunting period 3
	- col_wint_d2: Collar effect on survival winter hunting period 2
	- col_wint_d3: Collar effect on survival winter hunting period 3
	- col_spr_d2: Collar effect on survival spring hunting period 2
	- col_spr_d3: Collar effect on survival spring hunting period 3
	- pcap_heterog: Effect of lowly heterogeneous group on capture probability
	- pcap_sex: Effect of sex on pcap heterogeneity (beta female)
	- pcap: capture probability (1/summer occasion)
	- recov_fall1: recovery probability for fall during hunting period 1
	- recov_fall2: recovery probability for fall during hunting period 2
	- recov_fall3: recovery probability for fall during hunting period 3
	- recov_winter1: recovery probability for winter during hunting period 1
	- recov_winter2: recovery probability for winter during hunting period 2
	- recov_winter3: recovery probability for winter during hunting period 3
	- recov_sp_d2: recovery probability for spring during hunting period 2
	- recov_sd_d3: recovery probability for spring during hunting period 3
	- col_obs: collar observation probability (Index=174-202: summer, 204-233: fall, 235-264: winter, 266-294: spring)
	- het_obs_summer: effect of lowly observable group on observation probability for summer
	- het_obs_fall: effect of lowly observable group on observation probability for fall
	- het_obs_winter: effect of lowly observable group on observation probability for winter
	- het_obs_spring: effect of lowly observable group on observation probability for spring
	- band_recov_p1: effect of wearing a legband on recovery probability hunting period 1(vs. collars)
 	- band_recov_p2: effect of wearing a legband on recovery probability hunting period 2(vs. collars)
 	- band_recov_p3: effect of wearing a legband on recovery probability hunting period 3(vs. collars)
   - t: year (for survival estimates only)
	








 