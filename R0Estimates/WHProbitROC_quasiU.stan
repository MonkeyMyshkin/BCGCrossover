functions {
  vector quasi1(int N,real R0)
  {
  vector[N] p;  
  
  for(i in 1:N)
  {
    p[i]= (tgamma(1+N-1) / (i*tgamma(1+N-i)))*((R0/N)^(i-1));
  }
  p[1] = 1.0/sum(p);
  p[2:N]=p[2:N]*p[1];
  return(p);
  }

  vector quasi2(int N,real R0)
  {
  vector[N] p;  
  
  for(i in 1:N)
  {
    p[i]= (tgamma(1+N-1) / (tgamma(1+N-i)))*((R0/N)^(i-1));
  }
  p[1] = 1.0/sum(p);
  p[2:N]=p[2:N]*p[1];
  return(p);
  }

}

data {                      // Data block
  int<lower=1> G;           // Number of groups 
  int<lower=1> A;           // Number of animals
  int<lower=1> T;           // Number of tests
  int<lower=1> E;           // Number of terms in likelihood/response (2^T)
  int<lower=1> D;           // Number of unique diagnostic test groups
  
  int<lower=1> tind[T];     // Index to group that diagnostic test belongs to
  int<lower=1> gind[A];     // Index to group that animal belongs to
  int          y[A];        // Response - defined test response for each animal
  matrix[E,T]  ind;         // Indicator variable used to define response and likelihood
  int          herd_size[G]; // Size of each herd
 }

parameters {                          // Parameters block
 
 real<lower=-8,upper=3.5> R0_m;// R0 in each group
 real<lower=0> tau;
 vector<lower=-8,upper=8>[G] eta;
 
  vector<lower=-8,upper=0>[T] a0;     // Independence parameters (uninfected)
  vector<lower=0,upper=8>[D]  sigma;
}

// Use transformed parameter block to calculate sensitivity, specificity
transformed parameters {
vector<lower=0,upper=1>[T] sensitivity;
vector<lower=0,upper=1>[T] specificity = 1 - Phi(a0);
vector[A] log_lik;
vector[G] R0;

for(t in 1:T)
{
sensitivity[t] = Phi(inv_Phi(Phi(a0[t]))+sigma[tind[t]]);
}

for(g in 1:G)
{
  R0[g] = exp(R0_m + tau*eta[g]);
}

 // likelihood
  
  for(a in 1:A){

    vector[E] p;
    vector[herd_size[gind[a]]] lik_sum;
    vector[herd_size[gind[a]]] p_i;
    p_i=quasi1(herd_size[gind[a]],R0[gind[a]]);
    
    for(k in 1:herd_size[gind[a]])
    {
    real phi = 1.0*k/herd_size[gind[a]];
    for(e in 1:E)
    {
      real s_prod = 1;
      real p_prod = 1;
      
      for(t in 1:T)
      {
      s_prod = s_prod*(ind[e,t] * sensitivity[t] + (1-ind[e,t]) * (1-sensitivity[t]));
      p_prod = p_prod*(ind[e,t] * (1-specificity[t]) + (1-ind[e,t]) * specificity[t]);
      }
      
      p[e] = phi * (s_prod) + (1-phi) * (p_prod);

    }
    
    lik_sum[k] = log(p_i[k]) + bernoulli_lpmf(1 | p[y[a]]);
   
    }
    log_lik[a] = log_sum_exp(lik_sum);
}
}

model {// Model block

  a0 ~ normal(0,1);
  sigma ~ normal(0,1);
  
  R0_m ~ normal(0,1);
  tau  ~ exponential(1);
  eta ~ normal(0,1);
  
   for(a in 1:A)
   {
   target += log_lik[a];
   }
    
  }


