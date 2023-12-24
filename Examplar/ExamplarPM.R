SI_det <- function(t,y,p)
{
  beta = p[1]
  es = p[2]
  ei = p[3]
  IS = p[4]
  VIS = p[5]
  
  S = y[1]
  I = y[2]
  V = y[3]
  VI = y[4]
  
  N = sum(y) + IS + VIS
  
  lambda = beta*(I + IS + ei*(VI + VIS))/N
  
  dy <- numeric(2) 
  dy[1] <- -lambda*S
  dy[2] <-  lambda*S
  dy[3] <- -lambda*es*V
  dy[4] <-  lambda*es*V
  return(list(dy)) 
}

require(deSolve)

# Direct + Infectiousness vaccine effects

oot<-lsoda(c(15,0,15,0),seq(0,12,0.1),SI_det,parms=c(0.12,1.0-0.6,1.0-0.75,30,0.0)) 
colnames(oot) <- c('time','S','C','SV','V')
class(oot) <- NULL
oot <- as.tibble(oot)


examplar_exp <- oot %>% mutate(efficacy = 1-(V/(V+SV))/(C/(C+S))) %>% select(time,efficacy) %>% mutate(FOI='Low')

oot<-lsoda(c(15,0,15,0),seq(0,12,0.1),SI_det,parms=c(0.24,1.0-0.6,1.0-0.75,30,0.0)) 
colnames(oot) <- c('time','S','C','SV','V')
class(oot) <- NULL
oot <- as.tibble(oot)


examplar_exp <- examplar_exp %>% bind_rows(oot %>% mutate(efficacy = 1-(V/(V+SV))/(C/(C+S))) %>% select(time,efficacy) %>% mutate(FOI='Mid'))


oot<-lsoda(c(15,0,15,0),seq(0,12,0.1),SI_det,parms=c(0.48,1.0-0.6,1.0-0.75,30,0.0)) 
colnames(oot) <- c('time','S','C','SV','V')
class(oot) <- NULL
oot <- as.tibble(oot)


examplar_exp <- examplar_exp %>% bind_rows(oot %>% mutate(efficacy = 1-(V/(V+SV))/(C/(C+S))) %>% select(time,efficacy) %>% mutate(FOI='High'))

examplar_exp <- examplar_exp %>% mutate(FOI=factor(FOI,levels=c('Low','Mid','High')))

endpoint_fig <- ggplot(examplar_exp,aes(x=time,y=100*efficacy,col=FOI)) + geom_path(linewidth=2) + 
 xlab('Time (Months)') + ylab('% Endpoint Efficacy')  

ggsave(endpoint_fig, width=4.75, height=4.75*0.8,file="../Manuscript_figures/Fig_S1.pdf")
