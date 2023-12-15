#assess network features of observed and simulated networks
require(PAFit)
require(igraph)

#function to assess network features, 
#requires a directed network with columns "from", "to", and "Time"

network_features <- function(net)
{
  data.frame(transitivity=transitivity(net, type="global"),
             edge_density=ecount(net)/(vcount(net)*(vcount(net)-1)),
             diameter=diameter(net,directed=T,weights=NA),
             degree_centr = centr_degree(net,mode='in',normalized=T)$centralization,
             between_centr = centr_betw(net, directed=T, normalized=T)$centralization,
             n_edges=gsize(net) #total number of edges in network 
             )
  }

#specify the fitted ERBM model to be used in the simulation
sim_net_features<-function(ERGM_fit_net, #fitted network
                           t_sim) # time to simulate over
  {

oot <- mclapply(1:500,function(x)
  { 
  sim_edgelist <- simulate(ERGM_fit_net, 
                        nsim = t_sim,
                        sequential=TRUE,
                        output = "edgelist") 
  
  if (t_sim >1)  {
  #create single data frame, id corresponds to the simulated network
  sim_edgelist <- data.table::rbindlist(sim_edgelist, idcol = TRUE) %>% 
    rename(sim =.id) %>%
    relocate(sim, .after=last_col())
  }

sim_edgelist<-sim_edgelist %>% 
  rename(from=.tail, to=.head)

if (t_sim >1)  {
    
    #to assess the features of the simulated network over a time period equal to the observed network,
    #select first year to sample from out of the total number of simulated years
    samp_sim<-sample(seq(from=min(sim_edgelist$sim), to=max(sim_edgelist$sim), by=1), size=1)
    
    #subset network
    sim_edgelist<-sim_edgelist %>% 
      filter(sim == samp_sim)
}
                         
#number of farms in network  
ID = sim_edgelist %>% 
  select(from, to) %>% 
  pivot_longer(cols=c(from, to), values_to = "farm") %>% 
  distinct(farm) %>% 
  arrange(farm)

dictionary <- data.frame(orig_ID=ID$farm, code=1:nrow(ID))

#assign new ID as listed in dictionary to avoid missing node IDs
sim_edgelist$from = sapply(sim_edgelist$from,function(x){dictionary$code[as.character(dictionary$orig_ID) == x]})
sim_edgelist$to = sapply(sim_edgelist$to,function(x){dictionary$code[as.character(dictionary$orig_ID) == x]})

net <- graph_from_data_frame(d=sim_edgelist[, c("from", "to")], vertices=dictionary$code, directed=T)
net <- simplify(net, remove.multiple = F, remove.loops = T) 
net <- delete.vertices(net, degree(net)==0)

(network_features(net)) 

return(network_features(net)) 
})

oot<-bind_rows(oot)%>% 
  remove_rownames()

}


#estimate network features of the observed network as reference

#number of farms in the network  
ID = edgelist %>% 
  select(from, to) %>% 
  pivot_longer(cols=c(from, to), values_to = "farm") %>% 
  distinct(farm) 

observed_net<-edgelist[, c("from", "to", "time")] %>% 
  rename(Time="time")
observed_net <- graph_from_data_frame(d=observed_net, vertices=ID$farm,directed=T)
observed_net <- simplify(observed_net, remove.multiple = F, remove.loops = T) 
observed_net <- delete.vertices(observed_net, degree(observed_net)==0)

reference <- network_features(observed_net)%>% 
  remove_rownames()

reference_feat<-as.matrix(t(reference))
colnames(reference_feat)<-paste("observed net")

#function for extracting median of simulated features
med_sim_feat<-function(oot){ #where oot is the estimated network features
  oot_features<-matrix(c((median(oot$transitivity)), (median(oot$edge_density)), (median(oot$diameter)), (median(oot$degree_centr)), (median(oot$between_centr)), (median(oot$n_edges))#, (median(oot$attachment))), 5, 1)
  ), 6, 1)
  colnames(oot_features)<-paste("simulated net")
  rownames(oot_features)<-paste(c("transitivity", "edge density", "diameter", "degree_centrality", "between centrality", "total edges"#, "attachment"))
  ))
  return(oot_features)
}

t_obs = max(edgelist$time/365)+1 #time period of observed network data, +1 as starts from 0

#simulate movements over same time period as observed time period (fig1) and simulate 50 years (fig 2)

t_sim=c(1, ceiling(50/t_obs)) 

for(i.t_sim in seq_along(t_sim))
{
  
  #networks features of simulated network 
  ergm.sim.1.feat<-sim_net_features(ERGM_fit_net=ergm.fit, t_sim=i.t_sim)
    
  ergm.sim.1.feat.med<-med_sim_feat(ergm.sim.1.feat)

  network_feat_results1<-cbind(reference_feat, ergm.sim.1.feat.med)
  print(network_feat_results1)
    
  ergm.sim.1.feat <- ergm.sim.1.feat %>% 
    rename("Transitivity"="transitivity", "Edge density"="edge_density", "Diameter"="diameter", 
           "Degree centrality"="degree_centr", "Betweenness centrality"="between_centr", "Edges"="n_edges")
  
  
    png(paste('./transmission_model/figures/ERGM_net_features', i.t_sim, '_', reg, '.png', sep=""), pointsize=13, width=414*2, height=480*2)
    par(mfrow=c(3,2), family="serif", cex=1.5)
    for(i in 1:6)
    {
      truehist(unlist(ergm.sim.1.feat[,i]),col='darkgrey',xlab=names(ergm.sim.1.feat)[i], 
               cex.lab=1.5, cex.axis=1.2, 
               )
      abline(v=reference[i],col='red',lwd=3)
    }
    
    dev.off()
    
  }

