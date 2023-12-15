#visualise simulation results: farm level

#estimate number of farms in each compartment, where a farm is considered infected if 1 or more positive animals
farm_stats<-sim_network %>% 
  mutate( node_S=ifelse(S>0 & I==0 & V==0 & IV==0, 1, 0), 
          node_I=ifelse(I>0, 1, 0), 
          node_V=ifelse(V>0 & I==0 & IV==0, 1, 0),
          node_IV=ifelse(IV>0, 1, 0),
          node_I_IV=ifelse(IV>0|I>0, 1, 0)) %>%   #indicate if farm infected (I or IV)
  group_by(time, sim, intervention) %>% 
  summarize(total_S=sum(node_S), total_I=sum(node_I), total_V=sum(node_V), total_IV=sum(node_IV), total_I_IV=sum(node_I_IV)) %>% #estimate total number of farms in each compartment (I or IV under total_I_IV)
  mutate(farm_prev=(total_I_IV)/(total_S+total_V+total_I_IV)) #estimate farm prevalence

farm_stats_long<-farm_stats %>% 
  mutate(prop_S=total_S/(total_S+total_V+total_I_IV),
         prop_I=total_I/(total_S+total_V+total_I_IV),
         prop_V=total_V/(total_S+total_V+total_I_IV),
         prop_IV=total_IV/(total_S+total_V+total_I_IV),
         prop_inf=total_I_IV/(total_S+total_V+total_I_IV)) %>% 
  select(sim, time, prop_S, prop_I, prop_V, prop_IV, prop_inf, intervention) %>% 
  pivot_longer(cols=c(prop_S,prop_I,prop_V,prop_IV, prop_inf), names_to="state", values_to="proportion") %>%  
  mutate(state=fct_relevel(state, levels="prop_S","prop_I","prop_V","prop_IV", "prop_inf")) %>% 
  group_by(state, time, intervention)

#calculate median and 95% credible interval
farm_stats_summary <- farm_stats_long %>% 
  group_by(state, time, intervention) %>% 
  summarise(q0.025 = quantile(proportion, probs = 0.025),
            q0.500 = quantile(proportion, probs = 0.5),
            q0.975 = quantile(proportion, probs = 0.975))
