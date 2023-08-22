#visualise results from the transmission model by region

load(paste('./transmission_model/RData_files/sim_summary_stats_all_reg','.RData', sep=""))


#Animal level

prev_animals<-ggplot(data = pop_stats_summary_all_regions %>% 
                       mutate(region=factor(region, levels=c("Gondar", "Mekelle", "Hawassa"))) %>% 
                       mutate(intervention=factor(intervention, levels=c("no_vacc", "0", "0.25", "0.5", "indir_eff"))) %>%  
                       filter(!intervention %in% c("0.25", 0.5)) %>% #hash to include indirect efficacy scenarios of e_i = 25% and e_i = 50%
                       filter(state=="prop_inf"),
                     aes(x = time)) +
  geom_ribbon(aes(ymin = q0.025*100, 
                  ymax = q0.975*100, 
                  fill=intervention,
                  group=intervention), 
              alpha = 0.25,
              color = NA) +     
  geom_line(aes(y = q0.500*100, colour=intervention, group=intervention), alpha=1, linewidth=1.5) +   
  facet_grid(cols = vars(region)) +
  theme_bw() + 
  theme(text=element_text(size=34,  family="serif"),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        legend.text=element_text(size=30),
        strip.text = element_text(size = 34),
        legend.position="bottom")+
  labs(x ="\nTime (years)",    
       y="% bTB infected cattle\n") +
  scale_colour_manual(
    values = c("#F8766D", "#00BFC4", "#C77CFF"),   
    na.value="darkgray", 
    labels = c("0"="Direct efficacy only ", "0.25"="25%", "0.5"="50%", 
                "indir_eff" = "Direct + indirect efficacy",
                 no_vacc="No vaccination "))+
  scale_fill_manual(
    values = c("#F8766D", "#00BFC4", "#C77CFF"),  
    na.value="darkgray", 
    labels = c("0"="Direct efficacy only ", "0.25"="25%", "0.5"="50%", 
                         #c("0"="0%", "0.25"="25%", "0.5"="50%", 
                         "indir_eff" = "Direct + indirect efficacy",
               no_vacc="No vaccination "))+
  scale_y_continuous(limits=c(0, 100), expand = expansion(mult = c(0.01, 0.03)))+
  scale_x_continuous(breaks=seq(0,365*50, by=365*10), labels=function(x){x/365},  #set breaks to every 10 years, transform day label to years
                     limits=c(0, NA), expand = expansion(mult = c(0.00, 0.0)))+
  theme(legend.title=element_blank()) 

prev_animals

ggsave(paste('./transmission_model/figures/prop_animals_inf_median_all_regions_95CI', '.png', sep=""), width=16, height=8.5)

save(prev_animals, file=paste('./transmission_model/RData_files/fig_prev_animals', '.RData', sep=""))


#Farm level

prev_farms<-ggplot(data = farm_stats_summary_all_regions %>% 
                     filter(!intervention %in% c("0.25", 0.5)) %>% #hash to include indirect efficacy scenarios of e_i = 25% and e_i = 50%
                    mutate(region=factor(region, levels=c("Gondar", "Mekelle", "Hawassa"))) %>% 
                     mutate(intervention=factor(intervention, levels=c("no_vacc", "0", "0.25", "0.5", "indir_eff"))) %>% 
                     filter(state=="prop_inf"),
                   aes(x = time)) +
  geom_ribbon(aes(ymin = q0.025*100,  
                  ymax = q0.975*100, 
                  fill=intervention,
                  group=intervention), 
              alpha = 0.25,   
              color = NA) +     
  geom_line(aes(y = q0.500*100, colour=intervention, group=intervention), alpha=1, size=1.5) +  
  facet_grid(cols = vars(region)) +
  theme_bw() + 
  theme(text=element_text(size=34,  family="serif"),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        legend.text=element_text(size=30),
        strip.text = element_text(size = 34), 
        legend.position="bottom")+
  labs(x ="\nTime (years)",    
       y="% bTB infected farms\n") +
  scale_colour_manual(values = c("#F8766D", "#00BFC4", "#C77CFF"),   
    na.value="darkgray", 
   labels = c("0"="Direct efficacy only ", "0.25"="25%", "0.5"="50%", 
              "indir_eff" = "Direct + indirect efficacy",
               no_vacc="No vaccination "))+
  scale_fill_manual(values = c("#F8766D", "#00BFC4", "#C77CFF"),   
    na.value="darkgray", 
    labels = c("0"="Direct efficacy only ", "0.25"="25%", "0.5"="50%", 
               "indir_eff" = "Direct + indirect efficacy",
               no_vacc="No vaccination "))+
  scale_y_continuous(limits=c(0, 100), expand = expansion(mult = c(0.01, 0.03)))+
  scale_x_continuous(breaks=seq(0,365*50, by=365*10), labels=function(x){x/365}, #set breaks to every 10 years, transform day label to years
                     limits=c(0, NA), expand = expansion(mult = c(0.00, 0.0)))+
  theme(legend.title=element_blank()) 

prev_farms


ggsave(paste('./transmission_model/figures/prop_farms_inf_median_all_regions_95CI', '.png', sep=""), width=16, height=8.5)

save(prev_farms, file=paste('./transmission_model/RData_files/fig_prev_farms', '.RData', sep=""))
