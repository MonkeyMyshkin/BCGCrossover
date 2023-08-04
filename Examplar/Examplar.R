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

# Groups 1,2

# No vaccine efficacy

oot<-lsoda(c(15,0,15,0),seq(0,12,0.1),SI_det,parms=c(0.24,1.0,1.0,30,0.0)) 
colnames(oot) <- c('time','S','C','SV','V')
class(oot) <- NULL
oot <- as.tibble(oot)

examplar_exp <- oot %>% select(time,C,V) %>% pivot_longer(2:3, names_to = "Treatment") %>% mutate(effect='None', group=1)
examplar_exp <- examplar_exp %>% bind_rows(oot %>% select(time,C,V) %>% pivot_longer(2:3, names_to = "Treatment") %>% mutate(effect='None', group=2))
examplar_exp <- examplar_exp %>% bind_rows(oot %>% select(time,C,V) %>% pivot_longer(2:3, names_to = "Treatment") %>% mutate(effect='None', group=3))

# Direct only

oot<-lsoda(c(15,0,15,0),seq(0,12,0.1),SI_det,parms=c(0.24,1.0-0.6,1.0,30,0.0)) 
colnames(oot) <- c('time','S','C','SV','V')
class(oot) <- NULL
oot <- as.tibble(oot)

examplar_exp <- examplar_exp %>% bind_rows(oot %>% select(time,C,V) %>% pivot_longer(2:3, names_to = "Treatment") %>% mutate(effect='Susceptibility', group=1))
examplar_exp <- examplar_exp %>% bind_rows(oot %>% select(time,C,V) %>% pivot_longer(2:3, names_to = "Treatment") %>% mutate(effect='Susceptibility', group=2))
examplar_exp <- examplar_exp %>% bind_rows(oot %>% select(time,C,V) %>% pivot_longer(2:3, names_to = "Treatment") %>% mutate(effect='Susceptibility', group=3))

# Direct + Infectiousness

oot<-lsoda(c(15,0,15,0),seq(0,12,0.1),SI_det,parms=c(0.24,1.0-0.6,1.0-0.75,30,0.0)) 
colnames(oot) <- c('time','S','C','SV','V')
class(oot) <- NULL
oot <- as.tibble(oot)

examplar_exp <- examplar_exp %>% bind_rows(oot %>% select(time,C,V) %>% pivot_longer(2:3, names_to = "Treatment") %>% mutate(effect='Susceptibility + Infectiousness', group=1))
examplar_exp <- examplar_exp %>% bind_rows(oot %>% select(time,C,V) %>% pivot_longer(2:3, names_to = "Treatment") %>% mutate(effect='Susceptibility + Infectiousness', group=2))
examplar_exp <- examplar_exp %>% bind_rows(oot %>% select(time,C,V) %>% pivot_longer(2:3, names_to = "Treatment") %>% mutate(effect='Susceptibility + Infectiousness', group=3))

# Group 4 

# No vaccine efficacy

oot<-lsoda(c(15,0,15,0),seq(0,12,0.1),SI_det,parms=c(0.24,1.0,1.0,0,30.0)) 
colnames(oot) <- c('time','S','C','SV','V')
class(oot) <- NULL
oot <- as.tibble(oot)

examplar_exp <- examplar_exp %>% bind_rows(oot %>% select(time,C,V) %>% pivot_longer(2:3, names_to = "Treatment") %>% mutate(effect='None', group=4))

# Direct Only

oot<-lsoda(c(15,0,15,0),seq(0,12,0.1),SI_det,parms=c(0.24,1.0-0.6,1.0,0,20.0)) 
colnames(oot) <- c('time','S','C','SV','V')
class(oot) <- NULL
oot <- as.tibble(oot)

examplar_exp <- examplar_exp %>% bind_rows(oot %>% select(time,C,V) %>% pivot_longer(2:3, names_to = "Treatment") %>% mutate(effect='Susceptibility', group=4))

# Both

oot<-lsoda(c(15,0,15,0),seq(0,12,0.1),SI_det,parms=c(0.24,1.0-0.6,1.0-0.75,0,20.0)) 
colnames(oot) <- c('time','S','C','SV','V')
class(oot) <- NULL
oot <- as.tibble(oot)

examplar_exp <- examplar_exp %>% bind_rows(oot %>% select(time,C,V) %>% pivot_longer(2:3, names_to = "Treatment") %>% mutate(effect='Susceptibility + Infectiousness', group=4))



ggplot(examplar_exp %>% filter(effect=='None'),aes(x=time,y=value,col=Treatment)) + geom_path(size=2) + 
  facet_wrap(~group) + xlab('Time (Months)') + ylab('Cumulative Infections')  
ggplot(examplar_exp %>% filter(effect=='Susceptibility'),aes(x=time,y=value,col=Treatment)) + geom_path(size=2) + 
  facet_wrap(~group) + xlab('Time (Months)') + ylab('Cumulative Infections')
ggplot(examplar_exp %>% filter(effect=='Susceptibility + Infectiousness'),aes(x=time,y=value,col=Treatment)) + geom_path(size=2) + 
  facet_wrap(~group) + xlab('Time (Months)') + ylab('Cumulative Infections')



