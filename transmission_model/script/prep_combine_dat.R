#prepare simulated regional data to combine 

#add region to datasets

pop_stats_summary <- pop_stats_summary %>% 
  mutate(region=reg) %>% 
  ungroup()

farm_stats_summary <- farm_stats_summary %>% 
  mutate(region=reg) %>% 
  ungroup()

assign(paste('farm_stats_summary_', reg, sep=""), farm_stats_summary)
assign(paste('pop_stats_summary_', reg, sep=""), pop_stats_summary)

#create reduced dataset
rm(batches, bayes_eff, compartments, credible, dir_eff, E, edgelist, e_i, e_s, ergm.fit, ergm.fit.gof,
   ergm.sim.1.feat, ergm.sim.1.feat.med, est_pars, ERGM_net, ext_transfers, farm_stats, farm_stats_long, 
   farm_stats_summary, forward_sim, herdsize, i, initialise_state, i.t_sim, ID, indir_eff, itr, 
   med_sim_feat, mortality_rate, mov_rate, network_feat_results1, network_features, observed_net, 
   obs_prev, p, parsInf, pop_stats_long, pop_stats, pop_stats_summmary, R0_prevM, reference, reference_feat, sample_lpars, 
   sim_net_features, sim_network, sims, spec_gpars, t_obs, t_sim, testDat, total_tspan, traj_scenarios, 
   transitions, tspan)

#save data for selected region reg
save.image(file=paste('./transmission_model/RData_files/sim_summary_stats_', reg, '.RData', sep=""))
