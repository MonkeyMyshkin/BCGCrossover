#Specify scenarios for transmission model

#load posterior distribution of direct and indirect vaccine efficacy estimates from the natural transmission study
load("./transmission_model/RData_files/vacc_eff.RData") 

#function for credible interval
credible <- function(x){data.frame(y=median(x),ymin=quantile(x,0.025),ymax=quantile(x,0.975))}

#extract 95% credible interval of estimated direct and indirect vaccine efficacy from posterior distribution
#using the estimates from the DST1 test
CI_eff<-bayes_eff %>% 
  filter(test=="DST1", 
         imputation=="Default") %>% 
          group_by(efficacy) %>% 
          summarise(credible(value))

#posterior distribution of direct vaccine efficacy estimated using DST1 diagnostic test
dir_eff<-bayes_eff %>% 
  filter(test=="DST1", 
         imputation=="Default",
          efficacy=="Susceptibility",
          value >= as.numeric(CI_eff[CI_eff$efficacy=="Susceptibility", "ymin"]) &
          value <= as.numeric(CI_eff[CI_eff$efficacy=="Susceptibility", "ymax"]))

#posterior distribution of indirect vaccine efficacy estimated using DST1 diagnostic test
indir_eff<-bayes_eff %>% 
  filter(test=="DST1", 
         imputation=="Default", 
         efficacy=="Infectiousness",
         value >= as.numeric(CI_eff[CI_eff$efficacy=="Infectiousness", "ymin"]) &
          value <= as.numeric(CI_eff[CI_eff$efficacy=="Infectiousness", "ymax"]))




#Scenarios for the reduction in rate at which vaccinated individuals are infected compared to unvaccinated individuals
e_s=c(0, 1) #set direct efficacy of vaccination (0 when no vaccination, sample from posterior distribution of direct vaccine efficacy estimates when vaccination

#Reduction in infectiousness of vaccinated individuals that become infected compared to unvaccinated individuals
e_i=c(0, 0.25, 0.50, 1) #set values of indirect efficacy of vaccination to 0, 0.55, 0.5 and sample from posterior distribution of indirect vaccine efficacy estimates (1)

#set scenarios for the proportion of newborns vaccinated to 0 and 100%
p=c(0, 1)


