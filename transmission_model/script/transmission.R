#compartmental transmission model

#load package for transmission model
require(SimInf)

#specify functions for SimInf transmission model:

##Specify states of individuals (cattle) within a node (farm), where
#S = susceptible, I = infected, V = vaccinated, IV = infected vaccinated
#Bcum = cumulative births, Dcum= cumulative deaths, Icum=cumulative count of infected animals
compartments <- c("S", "I", "V","IV", "Bcum", "Dcum", "Icum")

##individual transitions betwen compartments within a node
##the left hand side of the first '->' is the initial state, 
#the right hand side of the last '->' is the final state
##transition between the two compartments are described between the two arrows

#p = proportion animals vaccinated
#br: birth rate, which is specified at the start of the simulation and kept constant throughout the simulation
#N0: number of animals in the herd at time 0
#e_s = reduction in susceptibility to infection of vaccinated animals compared to unvaccinated individuals
#e_i=reduction in infectiousness of vaccinated animals that become infected compared to unvaccinated individuals
# k = death rate, v = excess death rate in infected herds (set to 0 in this model but can be used) 
transitions <- c("@ -> (1-p)*br*N0 -> S+Bcum", 
                "S -> S > 0 ? (b0*(I + (1-e_i)*IV)*S)/(S+I+V+IV) : 0 -> I+Icum", 
                "@ -> p*br*N0 -> V+Bcum",
                "V -> V > 0 ? ((1-e_s)*b0*(I + (1-e_i)*IV) *V)/(S+I+V+IV) : 0 -> IV+Icum", 
                "S -> S > 0 ? S*k : 0 -> Dcum", 
                "I -> I > 0 ? I*(k+v) : 0 -> Dcum",
                "V -> V > 0 ? V*k : 0 -> Dcum",
                "IV -> IV > 0 ? IV*(k+v) : 0 -> Dcum")


#specify which compartment scheduled events act on
#1=susceptible, 2=infected, 3=vaccinated, 4=infected and vaccinated, 5=infected AND infected+vaccinated, 6=all compartments except cumulative counts
E <- matrix(c(1, 0, 0, 0, 0, 1, 
              0, 1, 0, 0, 1, 1, 
              0, 0, 1, 0, 0, 1,
              0, 0, 0, 1, 1, 1, 
              0, 0, 0, 0, 0, 0, 
              0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0),
            
            nrow = 7,
            ncol = 6, 
            byrow=TRUE, dimnames = list(c("S", "I", "V", "IV", "Bcum", "Dcum", "Icum"),
                                        c("1", "2", "3", "4", "5", "6")))

initialise_state <- function(R0, H0, AP0) 
  {
  
  #specify number of animals in each compartment at time 0 (state0)
  S0 = H0/R0
  I0 = H0-S0
  
  #set number of infected animals to 0 at farms where observed prevalence was 0
  I0[AP0==0]=0
  
  #set herd size to S0 for farms negative to bTB
  S0[AP0==0]=H0[AP0==0]
  
  #use S0 and I0 as probabilities to draw S0 and I0 from multinomial distribution for initial state
  init0 = t(sapply(1:length(H0),function(i){(rmultinom(1,size=H0[i],prob=c(S0[i],I0[i])))}))
  
  #specify initial state based on S0 and I0 drawn 
  state0 <- data.frame(S = init0[,1], 
                       I = init0[,2], 
                       V = rep(0, length(H0)),
                       IV = rep(0, length(H0)),
                       Bcum = rep(0, length(H0)),
                       Dcum = rep(0, length(H0)),
                       Icum  = rep(0, length(H0)),
                       Icum_rem = rep(0, length(H0)),
                       Myield = rep(0, length(H0)),
                       H = rep(0, length(H0)))
  #calculate herd size for each farm
  state0$H = rowSums(state0[,1:4])
  
  return(state0)
}

#SimInf forward simulation. For more information, please see 
#Widgren S, Bauer P, Eriksson R, Engblom S. SimInf: An R Package for Data-Driven Stochastic Disease Spread Simulations. J Stat Softw. 2019; 91:1–42.
forward_sim <- function(gpars, N0, b0, br, k, v, state0, tspan, scheduled_events){  
  model <- mparse(transitions = transitions, #transitions: left side is the initial state and right side is the state it transitions into
                  compartments = compartments, #compartments ("S", "I", "V", "IV")
                  ldata=data.frame(N0, b0, br, k, v), #node specific data
                  gdata = gpars,  # global data vector that is common to all farms
                  u0 = state0, #initial state in each node (individuals in each compartment)
                  tspan = tspan, #time points where the state of each node is returned
                  events = scheduled_events,  #scheduled events
                  E=E, #matrix to handle scheduled events
                  N=NULL) #shift_matrix, currently not in use
  
  result <- run(model = model, threads = 1)
  
  x<-trajectory(model=result)
  
  #calculate herd size for each farm
  x$H = rowSums(x[,c("S", "I", "V", "IV")])
  
  return(x)
  
}


#animal movements between farms. For more information, please see 
#Widgren S, Bauer P, Eriksson R, Engblom S. SimInf: An R Package for Data-Driven Stochastic Disease Spread Simulations. J Stat Softw. 2019; 91:1–42.
ext_transfers <-function(event, Time, source_node, dest_node, batch, proportion)
{
  movements=data.frame(
    event='extTrans',
    time = as.numeric(Time),
    node = as.numeric(source_node),
    dest = as.numeric(dest_node),
    n=batch,  #number of animals per movement
    proportion=proportion, #proportion of animals in a herd moved
    select=as.numeric(6), #compartments that an event operates on, which is controlled by the select value together with the E matrix
    shift=as.numeric(0))
}

#sample node specific data for simulation
sample_lpars <- function(parms, N){
  sims=max(parms$sim) #number of simulations in dataset
  data.frame(parms) %>% 
  filter(sim==sample(1:sims,1)) %>% #randomly select single simulation of posterior estimates of herd parameters
  sample_n(N, replace = TRUE, weight = NULL) %>% #sample herd parameters for each farm
  mutate(node=1:N) #assign farm id
}

#specify global parameters
spec_gpars <-function(e_s, e_i, p) {
  gpars<-c("e_s"=e_s,  #direct efficacy
           "e_i"=e_i, #indirect efficacy
           "p"=p) #proportion of newborn vaccinated
}


#function to set effective contacts (b0) and birth rate (br)
#u=removal rate, rho=net movement rate to/from farm, b0=effective contacts, R0=R0 for farm
est_pars<- function(u, rho, v, R0){
  b0 = R0*((u+rho)) #calculate b0 for each farm at endemic equilibrium
  br=(u+rho) #birth rates for farms without any infected animals
  br_I = br #birth rate for each infected farm at endemic equilibrium - same as birthrate for disease free farms in this model
  parms<-data.frame(b0, br, br_I)
  return(parms)
}


#function to estimate net animal export/import rate of animals from/to a farm
#N = total number of farms in a simulated network
#n_out = total number of animals exported (sold) from the farm
#n_in=total number of animals entering (purchased to) the farm
#H0 = target herd size at an endemic equilibrium
mov_rate<-function(N=N, ergm.sim=ergm.sim, H0=lpars$herdsize){
  animal_moves <- data.frame(farmID=1:N) %>% 
    left_join(ergm.sim %>% 
                group_by(from) %>% 
                summarise(n_out=sum(batch)), by=c("farmID"="from")) %>% 
    left_join(ergm.sim %>% 
                group_by(to) %>%
                summarise(n_in=sum(batch)), by=c("farmID"="to")) %>% 
    ungroup() %>% 
    replace_na(list(n_out=0, n_in=0)) %>%  #replace NA in entries for farms where no in/out movements with 0
    add_column(H0) %>% 
    mutate(rho=(n_out-n_in)/H0) %>% 
    mutate(rho=rho/(365*50)) %>%  #transform movement rate for 50 years to daily
    pull(rho)
  
  return(animal_moves)
}


#function to generate batch size (number of animals) moved with each scheduled movement:
#assign batch size to movements in simulated network from negative binomial distribution 
#of batch size (number of animals) per movement event from observed network


batches <- function(ergm.sim=ergm.sim, distr_batchsize=distr_batchsize, nodes=nodes, n_mov=n_mov)
{

#create empty dataframe to collect networks generated for each time block
mov_batch<-data.frame(i_mov = integer(), batch = integer())

for (i_mov in 1:n_mov){
  
  from = ergm.sim$from[i_mov]   #source farm for scheduled movement

  H_source<-nodes$H[from]   #herd size for selected source node
  
  #generate batch size for movement
  batch=as.integer(rnbinom(1, mu=distr_batchsize$estimate["mu"], size=distr_batchsize$estimate["size"])+1)
  
  #destination farm for movement
  to = ergm.sim$to[i_mov]
  
  #update herdsize of source node and destination node
  nodes$H[from] = nodes$H[from] - batch
  nodes$H[to] = nodes$H[to] + batch
  
  mov_batch = mov_batch %>% 
    add_row(i_mov, batch)
}

return(mov_batch)

}