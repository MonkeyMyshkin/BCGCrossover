#simulate stochastic transmission model with animal movements

#specify fitted ERGM to use in simulation
ergm.fit<-ergm.fit

#specify time period of simulation
t_obs = max(edgelist$time/365)+1 #time period of observed network data, +1 as starts from 0 

t_sim=ceiling(50/t_obs) #simulate 50 years

#simulate 50 years where the state of each farm is returned every 30 days 
tspan <- seq(from=1,to=365*50,by=30) 
total_tspan<- max(tspan)

#simulations to run for each intervention scenario
sims=1500

#simulate transmission
source("./transmission_model/script/sim_inf_network.R")

sim_network <- do.call(rbind, sim_inf_network)

#remove large datasets no longer needed 
rm(sim_inf_network)
gc()

#create new column for intervention used, where if vaccination was used the value indicates indirect vaccine efficacy used 
#inidr_eff = posterior estimates of the indirect vaccine efficacy from the natural transmission study 
sim_network<-sim_network %>% 
  mutate(intervention=ifelse(p==0 & e_i==0, "no_vacc", ifelse(p==1 & e_i==0, "0", ifelse(e_i==0.25, "0.25", ifelse(e_i==0.5, "0.5", "indir_eff"))))) 

#save data for selected region reg
save(sim_network, reg, file=paste('./transmission_model/RData_files/sim_network', '_', reg, '.RData', sep=""))

