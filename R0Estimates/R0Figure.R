require(tidyverse)
require(rstan)
require(loo)
require(tidybayes)
require(patchwork)

load('latentQuasi.RData')

credible<-function(x)
{
  z=quantile(x,c(0.025,0.5,0.975))
  return(data.frame(y=z[2],ymin=z[1],ymax=z[3]))
}



latent <- read.csv('TestAnon.csv')

# Aggregate "Dairy Belt" locations around Addis to be coded as "Addis Ababa"
latent$Site[!is.element(latent$Site,c("KOMBOLCHA","HAWASSA","MEKELLE"))] = 'ADDIS ABABA'


herd_sizes <- latent %>% group_by(g) %>% 
  summarise(N=length(g),site=unique(Site),round=unique(Round)) %>% 
  mutate(key=as.numeric(g))

R0_by_herd <- fit_whQuasiU %>% spread_draws(R0[g]) %>% 
  left_join(herd_sizes) %>% 
  mutate(model='Prevalence') %>% 
  select(-.chain,-.iteration,-.draw)

p2=ggplot(R0_by_herd,aes(x=N,y=R0))+ #col=site)) + 
  stat_summary(fun.data=credible) + 
  #scale_colour_brewer('Site',palette='Set2') + 
  scale_y_log10() + 
  ylab(expression(R[0])) + 
  xlab('Herd Size') + 
  geom_hline(yintercept = c(1.0/(1-0.558))) + 
  geom_hline(yintercept = c(1.0/(1-0.25),1.0/(1-0.74)),lty=2) +

  #Critical values of R0 for total vaccine coverage
  #geom_hline(yintercept = c(1.0/(1-0.89)),col='red') + 
  #geom_hline(yintercept = c(1.0/(1-0.75),1.0/(1-0.96)),lty=2,col='red') 
  

mean_percent <- function(x){100*mean(x)}
naive_errorL <- function(x){100*(mean(x)-sqrt(mean(x)*(1-mean(x)))/sqrt(length(x))) }
naive_errorU <- function(x){100*(mean(x)+sqrt(mean(x)*(1-mean(x)))/sqrt(length(x))) }


p1=ggplot(latent %>% 
            rename(SIT=PPDB,CCT=PPDBA) %>% 
            left_join(herd_sizes) %>% 
            pivot_longer(ends_with('T'),names_to='Test') ,aes(x=N,y=as.numeric(value),col=Test)) + 
 stat_summary_bin(fun=mean_percent,fun.min=naive_errorL,fun.max=naive_errorU,position=position_dodge(2))+xlab('Herd Size')+ylab('% Apparent Prevalence')

png(filename='Supp_R0Fig.png',width = 20,height=7.5,units='cm',res=600)
print(p1+p2)
dev.off()


