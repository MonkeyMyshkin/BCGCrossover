#create ERGM network object 
ERGM_net<-as.network(x=edgelist %>%  select(-batchsize), 
                     directed=TRUE,
                     multiple=TRUE) #multiple edges are allowed

#preferred model based on model selection for region reg
if (reg == "Gondar"){

ergm.fit<-ergm(ERGM_net ~ edges + 
               gwidegree(0.75, fixed = TRUE), #Geometrically weighted in degree with a decay of 0.75
               control = control.ergm(MCMC.burnin = 100000, MCMC.samplesize = 1000000)
  )
}

if (reg == "Hawassa"){
  
  ergm.fit<-ergm(ERGM_net ~ edges + 
                     gwidegree(0.5, fixed = TRUE)+ #Geometrically weighted in degree with a decay of 0.5
                     gwodegree(0.75, fixed = TRUE), #Geometrically weighted out degree with a decay of 0.5
                   control = control.ergm(MCMC.burnin = 100000, MCMC.samplesize = 1000000)
  )  
}  

  
  
if (reg == "Mekelle"){

ergm.fit<-ergm(ERGM_net ~ edges + 
                gwidegree(0.75, fixed = TRUE), # Geometrically weighted in-degree with a decay of 0.75
               control = control.ergm(MCMC.burnin = 100000, MCMC.samplesize = 1000000)
  )
}


#the fitted ERGM network used in the analysis is provided in 
paste('./transmission_model/RData_files/ergm.fit_', reg, '.RData', sep="")

