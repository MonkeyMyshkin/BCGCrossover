require(tidyverse)

mk_list_for_stan <- function(latent)
{
  
  # Reformat for latent class analysis, Two tests SIT, SICCT (Standard Interpretation)
  
  G=length(unique(latent$g))
  A=dim(latent)[1]
  T = 2
  E=2^T
  D = 1
  
  response_mask = expand.grid(c(0,1),c(0,1))
  
  # All animals
  # Encode test pattern for each animal
  y=sapply(1:dim(latent)[1],function(i){which(apply(response_mask,1,function(x,i){prod(i==x)},i=latent[i,5:6])==1)})
  
  latent_dat <- list(G=G,A=A,T=T,E=E,D=D,
                     tind=c(1,1),
                     gind=as.numeric(latent$g),
                     y=y,
                     ind=as.matrix(response_mask),
                     herd_size = unlist(latent %>% group_by(g) %>% summarise(n=length(g)) %>% select(n)))
  
  return(latent_dat)
}