#Mechanistic transmission model for the spread of bovine tuberculosis within the Ethiopian Dairy Sector

#Main script for generation of synthetic movement networks of cattle based on observed cattle movements in three study 
#areas in Ethiopia (Gondar, Mekelle, Hawassa) and simulation of within and between herd transmission of bovine 
#tuberculosis and assessment of the impact of vaccination on the prevalence of bovine tuberculosis

#load packages
source('./transmission_model/script/packages.R')

#The generation of a synthetic movement network and simulation of transmission of bovine tuberculosis was performed separately for
#the three study areas. Specify the study area of interest by unhashing the region:

#reg<-"Gondar"
#reg<-"Mekelle"
#reg<-"Hawassa"



###Cattle movement network model###

#load edgelist of cattle movements between farms for region reg
#from = source farm; to = destination farm; time = time point of movement from day 0; batchsize = number of cattle per movement 
edgelist<-read.xlsx(paste('./Data/edgelist_', reg, '.xlsx', sep=""))

#Fit Exponential Random Graph model (ERGM) based on observed movements of cattle in the area
source('./transmission_model/script/ERGM_network_fit.R')

#load the fitted ERGM for region reg
load(paste('./transmission_model/RData_files/ergm.fit_', reg, '.RData', sep=""))

#assess goodness of fit of the fitted network
source('./transmission_model/script/ERGM_gof.R')

#assess network features of simulated network compared to observed movement network
source('./transmission_model/script/ERGM_network_features.R')


###Within and between herd transmission model of bovine tuberculosis###

#prepare a dataframe of parameters for the transmission model simulation, 
#including herd estimates of R0, transmission parameters, herd size and mortality rate
source('./transmission_model/script/prep_prevM.R')

#compartmental transmission model
source("./transmission_model/script/transmission.R")

#specify transmission parameters

#specify vaccination scenarios
source('./transmission_model/script/scenarios.R')

#load transmission parameters for simulation
#sim - simulation, farm = farm identification number, R = herd specific R0, 
#herdsize = number of animals in the herd, u =  mortality rate, v = excess mortality rate in infected animals
load('./transmission_model/RData_files/parsInf.RData')#uses mortality estimates from normal distribution of observed mortality rate in Ethiopian herds and no excess mortality in infected animals

obs_prev<-read.xlsx(paste('./Data/obs_prev_', reg, '.xlsx', sep="")) #observed farm level prevalence in region reg

#simulate stochastic transmission model of bovine tuberculosis with animal movements
source("./transmission_model/script/net_transmission.R")

#visualise outputs

#visualise results at animal level
source("./transmission_model/script/vis_animal.R")

#visualise results at farm level
source("./transmission_model/script/vis_farm.R")

#save data to be combined
source("./transmission_model/script/prep_combine_dat.R")

#repeat the script above for all three regions, then continue below to visualise the outputs from the three regions together

source("./transmission_model/script/combine_reg_data.R")

#visualise results from the transmission model by region
source("./transmission_model/script/vis_combined_data.R")

#visualise estimated R0 and herd size by herd and region
source("./transmission_model/script/vis_R0.R")

#create a combined plot of the estimated R0 and results from the transmission model
source("./transmission_model/script/vis_manuscript.R")

