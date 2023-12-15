#combine regional datasets

#load transmission model results from the three regions

load("./transmission_model/RData_files/sim_summary_stats_Gondar.RData")
load("./transmission_model/RData_files/sim_summary_stats_Mekelle.RData")
load("./transmission_model/RData_files/sim_summary_stats_Hawassa.RData")

#combine results from the three regions
pop_stats_summary_all_regions<-bind_rows(pop_stats_summary_Gondar, pop_stats_summary_Mekelle, pop_stats_summary_Hawassa)

farm_stats_summary_all_regions<-bind_rows(farm_stats_summary_Gondar, farm_stats_summary_Mekelle, farm_stats_summary_Hawassa)

#save combined regional data
save(pop_stats_summary_all_regions, farm_stats_summary_all_regions, CI_eff, file=paste('./transmission_model/RData_files/sim_summary_stats_all_reg', '.RData', sep=""))
