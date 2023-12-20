#visualise estimated R0 and herd size by herd and region

#read R0 estimates combined with herd size
load('./transmission_model/RData_files/parsInf.RData') 

#read site data for R0 estimates
latent <- read.csv('./R0Estimates/TestAnon.csv')

#Aggregate farms around Addis Ababa to be coded as "Addis Ababa"
latent$Site[!is.element(latent$Site,c("KOMBOLCHA","HAWASSA","MEKELLE"))] = 'ADDIS ABABA'

latent <-latent %>% 
  mutate(Site=str_to_title(Site))

credible<-function(x)
{
  z=quantile(x,c(0.025,0.5,0.975))
  return(data.frame(y=z[2],ymin=z[1],ymax=z[3]))
}

herd_sizes <- latent %>% group_by(g) %>% 
  summarise(N=length(g),site=unique(Site),round=unique(Round)) %>% 
  mutate(key=as.character(g))

#read R0 estimates for model used
R0_by_herd <- parsInf %>% 
  left_join(herd_sizes %>% select(-g) , by=c("farm"="key"))

#save R0 by herd size
save(R0_by_herd, file=paste('./transmission_model/RData_files/R0_by_herd', '.RData', sep=""))

vis_R0_by_herd<-ggplot(R0_by_herd %>% 
                         mutate(site=factor(site, levels=c("Addis Ababa", "Mekelle", "Kombolcha", "Hawassa"))),
                       aes(x=N,y=R0, col=site)) + 
  stat_summary(fun.data=credible, size=1) + 
  scale_colour_manual(values=brewer.pal(5, "Dark2")[c(2:5)]) + 
  labs(colour='Site')+
  scale_y_log10() + 
  ylab(expression(R[0])) + 
  xlab(' \nHerd size') + 
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(), 
        text=element_text(size=34,  family="sans"),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30))+
  geom_hline(yintercept = c(1.0/(1-0.558))) + 
  geom_hline(yintercept = c(1.0/(1-0.25),1.0/(1-0.74)),lty=2)

save(vis_R0_by_herd, file=paste('./transmission_model/RData_files/vis_R0_by_herd', '.RData', sep=""))
