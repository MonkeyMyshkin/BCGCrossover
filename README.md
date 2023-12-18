Supplementary data and code for:

"BCG vaccination of cattle reduces transmission of bovine tuberculosis, improving the prospect of elimination" by Abebe Fromsa, Katriina Willgert, Sreenidhi Srinivasan, Getnet Mekonnen, Wegene Bedada, Balako Gumi, Matios Lakew, Biniam Tadesse, Berecha Bayissa, Asegedech Sirak, Musse Gima, Solomon Gebre, Tesfaye Rufael, Maroudam Veerasami, H. Martin Vordermeier, Douwe Bakker, Stefan Berg, Gobena Ameni, Nick Juleff,
Mart C.M. de Jong, James Wood, Andrew Conlan & Vivek Kapur with the ETHICOBOTS and ABTBC consortia.

shared under Creative Commons Attribution-ShareAlike 4.0 licence.

All Code written by Andrew J K Conlan and Katriina Willgert

This readme file gives a brief description of the original data and scripts necessary to reproduce the analyses and figures presented in the manuscript.

# Structure of repository and directory structure:

| Folder      | Description |
| ----------- | ----------- |
| Analysis    |  Rmarkdown file carrying out transmission model analysis of natural transmission experiment and generating estimates of vaccine efficacy      |
| Data   | All raw data sets and processed data tables used for analysis and model development (described below).        |
| Demography   | Rmarkdown file carrying out summary analysis of herd demographic data (age of tested animals) and estimating exponential rate of mortality within Ethiopia herds.        |
| Examplar   |  Script to generate examplar (expected) outcome of natural transmission study based on deterministic transmission model.       |
| Manuscript_figures   | Data figures generated for the manuscript are saved together in this folder.
| PM   |  Rmarkdown file carrying out analysis of endpoint vaccine efficacy using post-mortem (PM) data       |
| Posterior   |  Folder collecting data products from analysis (Vaccine efficacy estimates for DST1, DST10 tests)       |
| R0Estimates   |  stan code and R scripts to estimate within-herd reproduction ratios using tuberculin testing data and summary figure for manuscript.       |
| transmission_model   |  R scripts for development of meta-population transmission model and scenario analysis.       |

# Data

Original data tables and processed data products are stored in the "Data" subdirectory

## Natural Transmission Study Data 

Each animal is idenfited by three id's (Eartags ID1, ID2 and a unique numeric ID: Uni_ID). 
Treatment category encodes vaccine status: Control (C) or Vaccinate (V)).
Role category encodes whether the animal was a sentinel or seeder animal. (Note that sentinel animals in Phase I become seeder animals in phase II identifiable through their ID).

Animals were tested at two month intervals using a set of diagnostic tests (Dx):

IGRA: PPD-A, PPDB with difference PPD(B-A)
DST: DST1, DST0.1, DST0.01 (Different dilutions of the defined DST antigens)
Skin tests: SIT, SICCT (Single Intradermal Comparative Cervical Tuberculin test, described using contracted form SCT in manuscript), DST10

In Phase II, sentinel animals were grouped in two batches (I, II) for testing reflecting the staggered entry of these animals and different periods of exposure to seeder animals.

(Ante-mortem) Testing data for animals in Phase I:
"BCG Efficacy_G1 G2_Seeder Calves with Exit TST and IGRA Andrew Format .xlsx"
(Ante-mortem) Testing data for animals in Phase II:
"Updated OT_B-I and B-II_Combined_Andrew Format.xlsx"
Dates of tests and removals of animals in Phase II:
"PhaseII_dates.xlsx"
"PhaseII_removals.xlsx"
Pathology (Visible Lesion, VL) scores (post-mortem):
"VL Score of Seeders and Sentinels.xlsx"

These raw data tables were processed to make curated data tables used for the transmission model analysis by the "reshape_data.R" which generates the "TestingData.RData" R workspace.

## Ethicobots tuberculin testing data 

"TestAnon.csv"

Individual animal test results (logical TRUE or FALSE) for the SIT (coded as PPDB) and SCT (coded as PPDBA) grouped by herd (g). Herd identidies are anonymised, but stratified by the Site (ADDIS ABABA, SULUTA, SENDAFA, KOMBOLCHA, DEBRE-ZEIT, SEBETA, HAWASSA, MEKELLE, GONDAR, HOLETA) and study ID (1 (Ethicobots Round 1), 2 (Ethicobots Round 2), Firdessa, NAHDIC) relating to when and where the tests were carried out.

## Ethicobots herd demography 

"ethicobots_demo.csv"

Age of all animals collected alongside tuberculin testing data (and apparent herd prevalence herdprev)

## Ethicobots cattle movement data 

Anonymised edgelists for individual animal movements reported between herds from subset of herds in Ethicobots study from Gondar, Hawassa and Mekelle:

"edgelist_Gondar.xlsx"
"edgelist_Hawassa.xlsx"
"edgelist_Mekelle.xlsx"

## Point (herd level) apparent prevalence within subset of herds used for network model 

"obs_prev_Gondar.xlsx"
"obs_prev_Hawassa.xlsx"
"obs_prev_Mekelle.xlsx"

# Dependencies, data products and order of execution

Analyses must be carried out in order as data products from earlier analysis (estimation of vaccine efficacy, transmission rates) flow into the meta-population transmission model used for scenario analysis.

| Script                    | Depends on                                                                           | Generates Data Products        | Figures           |
|-------------------------- |--------------------------------------------------------------------------------------|--------------------------------|-------------------|
| Data/reshape_data.R       | Data/BCG Efficacy_G1 G2_Seeder Calves with Exit TST and IGRA Andrew Format .xlsx   | Data/TestingData.RData       |                   |
|                           | Data/Updated OT_B-I and B-II_Combined_Andrew Format.xlsx                           |                                |                   |
|                           | Data/PhaseII_dates.xlsx                                                            |                                |                   |
|                           | Data/PhaseII_removals.xlsx                                                         |                                |                   |
| Analysis/Efficacy_Analysis.Rmd   | ./Data/TestingData.RData              |  transmission_model/RData_files/vacc_eff.RData                              | Fig_S3, Fig_2     |
|                           |                                       | Posterior/dst_post.RData | | |
|                           |                                       | Posterior/dst1_post.csv | | |  
|                           |                                       | Posterior/dst10_post.csv | | |
| PM/PM Data.Rmd            | Data/TestingData.RData | ||
|                           | Data/VL Score of Seeders and Sentinels.xlsx |||
| Demography/Demography.Rmd | Data/ethicobots_demo.csv            | Demography/MortalityExp.csv | | | 
| R0Estimates/LatentQuasi.R | Data/TestAnon.csv | R0Estimates/LatentQuasi.RData | | | 
|                           |                     | R0Estimates/R0Estimates.csv | | | 
| R0Estimates/R0Figure.R    | Data/TestAnon.csv | | Fig_S6 |
|                           | R0Estimates/latentQuasi.RData ||| 
| transmission_model/script/main.R | Data/edgelist_Gondar.xlsx || Fig_3|
| | Data/edgelist_Hawassa.xlsx |||
| | Data/edgelist_Mekelle.xlsx |||
| | Data/obs_prev_Gondar.xlsx |||
| | Data/obs_prev_Hawassa.xlsx |||
| | Data/obs_prev_Mekelle.xlsx |||
| | R0Estimates/R0Estimates.csv |||
| | R0Estimates/TestAnon.csv |||
| | transmission_model/RData_files/vacc_eff.RData |||


