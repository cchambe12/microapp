// Microclimates Analysis
// 30 Jan 2020 - Started by Cat
// Level: Species on INTERCEPTS and SLOPES

data {
  
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] prov; 	// provenance predictor
	vector[N] method; 	// method predictor
		
	}
	
transformed data {
  vector[N] inter_provmethod;                 

  inter_provmethod = prov .* method; 
}

parameters {
  
  real mu_a_sp;   
  real mu_b_prov_sp;     
  real mu_b_method_sp;
  real mu_b_pm_sp; // slope of prov x method effect
  
  real<lower=0> sigma_b_prov_sp;
  real<lower=0> sigma_b_method_sp;
  real<lower=0> sigma_b_pm_sp;
  real<lower=0> sigma_a_sp;
  real<lower=0> sigma_y; 
  
  real a_sp[n_sp]; // intercept for species
  
  vector[n_sp] b_prov_ncp; // slope of prov effect 
  vector[n_sp] b_method_ncp; // slope of method effect 
  vector[n_sp] b_pm_ncp;
  
	}

transformed parameters {
  vector[N] yhat;

  vector[n_sp] b_prov = mu_b_prov_sp + sigma_b_prov_sp*b_prov_ncp; 
  vector[n_sp] b_method = mu_b_method_sp + sigma_b_method_sp*b_method_ncp; 
  vector[n_sp] b_pm = mu_b_pm_sp + sigma_b_pm_sp*b_pm_ncp;
  
  for(i in 1:N){    
    yhat[i] = a_sp[sp[i]] + // indexed with species
		          b_prov[sp[i]] * prov[i] + 
		          b_method[sp[i]] * method[i] +
		          b_pm[sp[i]] *  inter_provmethod[i];
	      }
	      
}

model {
	b_prov_ncp ~ normal(0, 1);
	b_method_ncp ~ normal(0, 1);
	b_pm_ncp ~ normal(0, 1);
	
	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	target += normal_lpdf(to_vector(b_prov) | 0, 50);
	target += normal_lpdf(to_vector(b_method) | 0, 50);
	target += normal_lpdf(to_vector(b_pm) | 0, 50);
	      
        mu_a_sp ~ normal(300, 50);
        sigma_a_sp ~ normal(0, 30);

        /*mu_b_prov_sp ~ normal(0, 50);
        sigma_b_prov_sp ~ normal(0, 20);
        
        mu_b_method_sp ~ normal(0, 50);
        sigma_b_method_sp ~ normal(0, 20);
        
        mu_b_pm_sp ~ normal(0, 50);
	      sigma_b_pm_sp ~ normal(0, 20);*/
        
        sigma_y ~ normal(0, 30);
	      
	y ~ normal(yhat, sigma_y);

}

/*generated quantities{
   real y_ppc[N];
   for (n in 1:N)
      y_ppc[n] = a_sp[sp[n]] + 
		b_prov[sp[n]] * prov[n] +
		b_method[sp[n]] * method[n];
    for (n in 1:N)
      y_ppc[n] = normal_rng(y_ppc[n], sigma_y);

}*/