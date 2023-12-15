#simulate transmission

#create empty list where can store model trajectories
traj_scenarios<-list()

#record iteration in itr
itr<-1

sim_inf_network<-lapply(1:sims,function(itr)
{
  
  
  ##########
  
  #simulate movement network
  
  #simulate movement network from fitted ERGM of observed movements
  ergm.sim <- simulate(ergm.fit, 
                       nsim = t_sim,
                       sequential=TRUE, #the end of one simulation is used as the start of the next
                       output = "edgelist") 
  
  #create single data frame, id corresponds to the simulated time period
  ergm.sim <- data.table::rbindlist(ergm.sim, idcol = TRUE) %>% 
    rename(from=.tail, to=.head, sim =.id)%>% 
    relocate(sim, .after=last_col())
  
  
  #########
  
  #timing of movement
  
  #for each simulated time block, randomly assign day of movement
  
  #create ermpty dataframe to collect networks generated for each time block
  sim_time<-data.frame(i_sim = integer(), Time=integer())
  
  for (i_sim in 1:t_sim){
    
    #number of movements in simulated network for selected simulation
    n_mov<-ergm.sim %>% 
      filter(sim==i_sim) %>% 
      summarise(total_mov=n()) %>% 
      pull(total_mov)
    
    #start and end of time period considered in loop in days, where i_repeat is the ith loop performed
    start_tspan=(t_obs*365*i_sim)-t_obs*365+1
    end_tspan=t_obs*365*i_sim
    
    #time point of movement event generated from uniform distribution over time period specified between min and max 
    Time=round(runif(n_mov, min=start_tspan, max=end_tspan))
    
    sim_time <- sim_time %>% 
      add_row(i_sim, Time) 
    
  }
  
  #add time of movements to simulated network
  ergm.sim<-ergm.sim %>% 
    mutate(Time=sim_time$Time) %>% 
    arrange(Time) %>% 
    mutate(movID=1:n()) #movement number
  
  
  #############
  
  #farms in simulated network
  
  nodes <- ergm.sim %>% 
    select(from, to) %>% 
    pivot_longer(cols=c(from, to), values_to = "farmID") %>% 
    distinct(farmID) %>% 
    arrange(farmID)
  
  #create dictionary of farm IDs and assign new ID to farm to avoid missing node IDs
  dictionary <- data.frame(orig_ID=nodes$farmID, code=1:nrow(nodes))
  
  #assign new ID to farms as listed in dictionary 
  ergm.sim$from = sapply(ergm.sim$from,function(x){dictionary$code[as.character(dictionary$orig_ID) == x]})
  ergm.sim$to = sapply(ergm.sim$to,function(x){dictionary$code[as.character(dictionary$orig_ID) == x]})
  nodes$farmID = sapply(nodes$farmID,function(x){dictionary$code[as.character(dictionary$orig_ID) == x]})
  
  
  ##########
  
  #assign transmission parameters to farm
  
  #total number of farms in simulated network:
  N <- nodes %>% 
    summarise(total_farms=n()) %>% 
    pull(total_farms)
  
  #sample local parameters (herd specific) for simulation
  #sims indicate which simulation from parms that the parameters are drawn from
  lpars<-sample_lpars(parms=parsInf, N=N) #%>%  


  ###########
  
  #batch size
  
  nodes <- nodes %>% 
    mutate(H0=lpars$herdsize, H=H0)  #H used in check of herd size before movement of batch. Initially set to H0
  
  #assign batch size to movements in simulated network from negative binomial distribution of batchsize (number of animals) per movement event from observed network
  
  #total movements in simulated network
  n_mov<-ergm.sim %>% 
    summarise(total_mov=n()) %>% 
    pull(total_mov)
  
  #fit negative binomial distribution of batchsize (number of animals) per movement event from observed network
  if(reg=="Mekelle"){
    #remove outlier in batchsize for Mekelle (22 animals purchased by vet school)
    distr_batchsize<-fitdistr(edgelist$batchsize[edgelist$batchsize!=22], "negative binomial")
  }else{
    distr_batchsize<-fitdistr(edgelist$batchsize, "negative binomial")
  }
  
  mov_batch<-batches(nodes=nodes, ergm.sim=ergm.sim, distr_batchsize=distr_batchsize, n_mov=n_mov)
  
  #add batch size for movements to simulated network
  ergm.sim<-ergm.sim %>% 
    left_join(mov_batch, by = c("movID" = "i_mov"))
  
  #estimate daily net movement rate to/from farms
  rho<-mov_rate(N=N, ergm.sim=ergm.sim, H0=lpars$herdsize)
  
  #estimate b0 and birth rate adjusted for net movement rate  (br) and birthrate adjusted for net movement rate and excess mortality/removal on infected farms (br_I)
  parms<-est_pars(rho=rho, u=lpars$u, v=lpars$v, R0=lpars$R0) 
  
  #observed between herd prevalence used to randomly assign if bTB positive or negative farm 
  AP0<-numeric(length=N)
  
  positive_farms<-sample(N, size=N*obs_prev$between_farm, replace=FALSE)
  
  #set farms randomly selected as positive to positive at time 0 in stochastic model:
  AP0[positive_farms]=1
  
  #fix birthrate to u+rho in herds without any infected animals 
  #set birthrate to account for excess mortality in infected herds if excess mortality used in model
  br<-parms$br #birth rate if bTB free farm at start of simulation
  br[positive_farms]=parms$br_I[positive_farms] #bTb positive farm at start of simulation
  
  #set starting state (state0)
  state0<-initialise_state(R0=lpars$R0, H0=lpars$herdsize, AP0)

  #for a specific iteration, run scenarios using same movement network, time of movements, batch size, 
  #sampled herd parameters (R0, herd size, mortality rate), initial infected farms and animals, and vaccine efficacy
  
  #stochastic model with movement network
  for(i.e_s in seq_along(e_s))
  {
    
    e_s_sim<-ifelse(e_s[i.e_s]==0, 0, sample(dir_eff$value, 1)) #if e_s=0 --> no vaccination, otherwise sample from posterior distribution of direct vaccine efficacy
    
    for(i.e_i in seq_along(e_i))
    {
      
      e_i_sim<-ifelse(e_i[i.e_i]!=1, e_i[i.e_i], sample(indir_eff$value, 1)) #if e_i==1 --> sample from posterior distribution of indirect vaccine efficacy, else use e_i
      
      for(i.p in seq_along(p))
        
          {
              
              #since if p=0, no animals are vaccinated, skip iteration if p=0 (i.e_p[1]) and e_s>0 (i.e. e_s[>1]) or e_i is >0  (i.e. i.e_i[>1]) or if p=1 (i.p[2]) but e_s=0 (i.e_s[1])
              if((i.p==1 & (i.e_s>1 |i.e_i>1)) | (i.p==2 & i.e_s==1))
                next
              
              #global parameters (shared between all populations)
              gpars<-spec_gpars(e_s=e_s_sim, e_i=e_i_sim, p=p[i.p])
      
              state0_sim<-state0 #set the initial starting condition that is used in the simulation and then updated when continue a simulation
              
              #run simulation over tspan, extracting status of herds monthly and calculating proportion of animals moved based on updated herd sizes
              
              #create empty dataframe to add results to and set up starting conditions
              inf_network<-data.frame()
              start_time<-1
              break_time<-0
              N0=state0$H #since state0 updated when simulation paused, create variable for starting herd size for constant birth rate with starting population size
              
              for(i_months in 1:(length(tspan)-1)){ #break monthly
               
                break_time <- i_months*30 #break monthly
               
                #add time point of simulation break to tspan_sim
                tspan_sim<-sort(c(tspan, start_time, break_time)) %>% 
                  unique()
                
                #extract scheduled movements during time period
                mov=ergm.sim %>% filter(Time>=start_time & Time<=break_time) #%>% 
                
                #check if scheduled time period has any scheduled movements. If not, set to null
                if(nrow(mov)==0){ 
                  movements<-NULL
                }else{
                  
                  #nodes with movements scheduled in time period
                  nodes_mov <- mov %>% 
                    select(from, to) %>% 
                    pivot_longer(cols=c(from, to), values_to = "farmID") %>% 
                    distinct(farmID) %>% 
                    arrange(farmID)
                  
                  nodes_mov<-nodes_mov %>% 
                    add_column(H=state0 %>% #number of animals in farm at break time
                                 slice(nodes_mov$farmID) %>% 
                                 pull(H))
                
                  #create empty dataframe to collect batch size in proportions for the time block
                  mov_batch<-data.frame(i_mov = integer(), movID = integer(), batch = integer(), proportion=numeric(), H_from=integer(), H_to=integer())
                  
                  for(i_mov in 1:nrow(mov)){
                    
                    movID<-mov$movID[i_mov]
                    from = mov$from[i_mov]  
                    to<-mov$to[i_mov] 
                    
                    #herd size of source farm and destination farm at time point
                    H_from=nodes_mov %>% filter(farmID==from) %>% pull(H)
                    H_to=nodes_mov %>% filter(farmID==to) %>% pull(H)
                    
                    #if 10 or less animals remaining in source herd if the batch size is removed, 
                    #set the batch size to next batch size possible while leaving 10 animals in the herd
                    if(H_from-mov$batch[i_mov]<=10){
                      
                      mov$batch[i_mov]=ifelse(H_from<10, 0, max((0:mov$batch[i_mov])[(H_from-0:mov$batch[i_mov])>=10]))
                      
                    }
                
                    #calculate proportion animals to be moved from the source farm based on batch size
                    proportion=round(mov$batch[i_mov]/H_from, 8)
                    
                    #add proportion to the dataframe of batch size
                    mov_batch = mov_batch %>% 
                      add_row(i_mov, movID=movID, batch=mov$batch[i_mov], proportion=proportion, H_from=H_from, H_to=H_to)
                    
                    #update herd size of source node and destination node
                    nodes_mov[nodes_mov$farmID==from, "H"] = H_from - mov$batch[i_mov]
                    nodes_mov[nodes_mov$farmID==to, "H"] = H_to + mov$batch[i_mov]
                    
                  }
                  
                  #add proportion of animals to be moved from herd to scheduled movements
                  mov<-mov %>% 
                    left_join(mov_batch %>% select(movID, proportion), by="movID")
                  
                  movements<-ext_transfers(Time=mov$Time, source_node=mov$from, dest_node=mov$to, batch=0, proportion=mov$proportion) #uses proportion of animals out of the total number of animals in the herd for movements 
                  
                }
                
                
                #forward simulation of infection network from the previous break point + 1 day (since time point of pausing simulation has already run) until the day before the next scheduled break_time
                inf_network_cont<-forward_sim(gpars=gpars, N0=N0, b0=parms$b0, br=br, k=lpars$u, v=lpars$v, state0=state0_sim, tspan=tspan_sim[tspan_sim>=start_time & tspan_sim<=break_time & tspan_sim<=total_tspan], scheduled_events=rbind(movements)) 
                
                inf_network<-bind_rows(inf_network, inf_network_cont)
                
                #update start time
                start_time<-break_time+1
                
                #update state0 to state the time of breaking for the next time simulation period
                state0_sim<- inf_network_cont %>% 
                  filter(time==break_time) %>% 
                  select(-node, -time)
                
              }
              
              #add global parameters for simulation and iteration
              inf_network<-cbind(inf_network, e_s=rep(gpars["e_s"], times=nrow(inf_network)), e_i=rep(gpars["e_i"], nrow(inf_network)), p=rep(gpars["p"], nrow(inf_network)), sim=itr)
              
              traj_scenarios<- rbind(traj_scenarios, inf_network) 
              
            }
          
        }
  }
  
  #update simulation
  itr<-itr+1
  
  return(traj_scenarios)})

gc()