functions{
  
// As defined in Brooks Pollock et al. 
// m - mortality rates for each age-group (mu, vector of length equal to age groups)
// i - index of age group
// age_widths is vector of length equal to number of age_groups given width in years

real Psi(vector u,int i,vector age_widths)
{
if(i==1)
{
return((1-exp(-u[1]*age_widths[1]))/u[1]);
}else{
real uin = sum(age_widths[1:(i-1)].*u[1:(i-1)]);
real ui = sum(age_widths[1:i].*u[1:i]);

real p=(exp(-uin)-exp(-ui))/u[i];
return(p);
}
}

}  
  

data {                      
  int<lower=1> no_herds;                   // Number of animals
  int          age_bins;            // Number of age bins
  vector[age_bins+1] breaks;        // breaks rate for each age bin
  vector[age_bins] widths;   // width of each bin
  int   counts[no_herds,age_bins]; // Age distribution of herds
}

parameters {                
  real<lower=0>           u0;
}

transformed parameters {      
  
  vector<lower=0>[age_bins]           u;
  vector<lower=0,upper=1>[age_bins]     p;
  
  for(a in 1:(age_bins))
  {
    u[a] = u0;
  }
  
 for(a in 1:(age_bins)){
		p[a] = Psi(u,a,widths);
  }
  p = p/sum(p);
}

model {                     
  // Priors
  u0 ~ cauchy(0,5);

  // likelihood
  for(k in 1:no_herds)
  {
  counts[k] ~ multinomial(p);
  }
}

generated quantities {
  vector[no_herds] log_lik;
  for(k in 1:no_herds)
  {
  log_lik[k] =  multinomial_lpmf(counts[k] | p);
  }
}

