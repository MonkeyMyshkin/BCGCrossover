if(!require(tidyverse)){install.packages('tidyverse')}

load('TestingData.RData')

df = phaseI_calves %>% 
  filter((date=='Pre-BCG' | date=='Post-BCG') & Dx=='PPD(B-A)') %>% 
  bind_rows(phaseII_calves %>% filter((date=='Pre-BCG' | date=='Post-BCG') & Dx=='PPD(B-A)'))

df = df %>% mutate(Group = factor(Group),Treatment = factor(Treatment,labels=c('Unvaccinated','Vaccinated')))


p1 = ggplot(df,aes(x=date,y=value,col=Treatment,shape=Group)) + 
  geom_point(position=position_dodge(0.5)) + ylim(c(-2,1)) + ylab('Delta OD450nm')



df = phaseI_calves %>% 
  filter(date=='Pre-BCG' & Dx=='PPD(B-A)') %>% 
  bind_rows(phaseII_calves %>% filter(date=='Pre-BCG' & Dx=='PPD(B-A)'))

df = df %>% mutate(Group = factor(Group),Treatment = factor(Treatment,labels=c('Unvaccinated','Vaccinated')))

p2 = ggplot(df,aes(x=Treatment,y=value,col=Treatment,shape=Group)) + 
  geom_point(position=position_dodge(0.5)) + 
  ylim(c(-2,1)) + ylab('Delta OD450nm') + 
  geom_hline(yintercept=0.0,linetype=2,col='red')

