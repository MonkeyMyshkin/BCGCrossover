#prepare a dataframe of parameters for the transmission model simulation, 
#including herd estimates of R0, transmission parameters, herd size and mortality rate

#read field estimates of R0 in Ethiopian dairy herds
R0_prevM<-read.csv(file = './R0Estimates/R0Estimates.csv')

#anonymous test data for each animal. g=farm
testDat<-read.csv(file = './R0Estimates/TestAnon.csv')


#Mean and standard deviation of posterior estimates of the mortality rate of all herds included 
#in the ETHICOBOTS study (see BCGCrossover/Demography)
mortality_rate<-data.frame(mean=0.2731918, sd=0.004857313)

#herdsize of tested farms
herdsize<-testDat %>% 
  group_by(g) %>% 
  summarise(herdsize=n()) %>% 
  rename(farm=g) %>% 
  mutate(farm=as.character(farm))


#Dataframe of estimated R0 and herd size by herd combined with mortality rate
parsInf<-R0_prevM %>% 
 mutate(sim=as.character(1:n()), .before=1) %>% 
 pivot_longer(cols=-sim, names_to="farm", values_to="R0") %>% 
 mutate(farm=str_remove(farm, "R0.")) %>% 
  left_join(herdsize, by="farm") %>%  #add herdsize
  #sample annual mortality rate from a normal distribution of mortality rates estimated from dairy herds in Ethiopia 
  mutate(u=rnorm(n(), mean=mortality_rate$mean, sd=mortality_rate$sd)) %>% 
  mutate(u=u/365) %>% #convert from annual rates to daily rates
  mutate(v=0, #there is no excess mortality attributed to disease in the model used and, as a result, v is set to 0
        )

#save data 
save(parsInf, file='./transmission_model/RData_files/parsInf.RData')
