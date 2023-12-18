require(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
require(loo)
require(parallel)
require(tidyverse)
require(tidybayes)

latent <- read.csv('../Data/TestAnon.csv')

source('./mk_list_for_stan.R')

latent_dat <- mk_list_for_stan(latent)

fit_whQuasiU <- stan(file = './WHProbitROC_quasiU.stan', data = latent_dat, chains = 4, 
                     control = list(adapt_delta = 0.8),iter=2000)

save(fit_whQuasiU,file='LatentQuasi.RData')

log_lik_whQuasiU <- extract_log_lik(fit_whQuasiU,c('log_lik'),merge_chains=FALSE)
loo_whQuasiU <- loo(log_lik_whQuasiU, r_eff=relative_eff(exp(log_lik_whQuasiU)),cores=4,save_psis = TRUE)

save(fit_whQuasiU,log_lik_whQuasiU,loo_whQuasiU,latent,latent_dat,file='LatentQuasi.RData')

write.csv(rstan::extract(fit_whQuasiU,pars='R0'),file='R0Estimates.csv',row.names=FALSE,col.names=FALSE)
