#visualise simulation results: animal level

#prevalence at animal level:
pop_stats<-sim_network %>% 
  group_by(time, sim, intervention) %>% 
  summarize(total_S=sum(S), total_I=sum(I), total_V=sum(V), total_IV=sum(IV), total_Icum=sum(Icum)) %>% 
  mutate(total_I_IV=total_I+total_IV) %>% #estomate total number of infected animals
  mutate(animal_prev=(total_I+total_IV)/(total_S+total_I+total_V+total_IV)) #estimate animal level prevalence

pop_stats_long<-pop_stats %>% 
  mutate(prop_S=total_S/(total_S+total_I+total_V+total_IV),
         prop_I=total_I/(total_S+total_I+total_V+total_IV),
         prop_V=total_V/(total_S+total_I+total_V+total_IV),
         prop_IV=total_IV/(total_S+total_I+total_V+total_IV),
         prop_inf=total_I_IV/(total_S+total_I+total_V+total_IV)) %>% 
  select(sim, time, prop_S, prop_I, prop_V, prop_IV, prop_inf, intervention) %>% 
  pivot_longer(cols=c(prop_S,prop_I,prop_V,prop_IV, prop_inf), names_to="state", values_to="proportion") %>% 
  mutate(state=fct_relevel(state, levels="prop_S","prop_I","prop_V","prop_IV", "prop_inf")) %>% 
  group_by(state, time, intervention)

#calculate median and 95% credible interval
pop_stats_summary <- pop_stats_long %>% 
  summarise(q0.025 = quantile(proportion, probs = 0.025),
            q0.500 = quantile(proportion, probs = 0.5),
            q0.975 = quantile(proportion, probs = 0.975))
