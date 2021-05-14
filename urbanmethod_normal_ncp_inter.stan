// Microclimates Analysis
// 30 Jan 2020 - Started by Cat
// Level: Species on INTERCEPTS and SLOPES

data {
  
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] urban; 	// urban predictor
	vector[N] method; 	// method predictor
		
	}
	
transformed data {
  vector[N] inter_urbanmethod;                 

  inter_urbanmethod = urban .* method; 
}

parameters {
  
  real mu_a_sp;   
  real mu_b_urban_sp;     
  real mu_b_method_sp;
  real mu_b_um_sp; // slope of urban x method effect
  
  real<lower=0> sigma_b_urban_sp;
  real<lower=0> sigma_b_method_sp;
  real<lower=0> sigma_b_um_sp;
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_y; 
  
  real a_sp[n_sp]; // intercept for species
  //vector[n_sp] a_sp_ncp;
  
  vector[n_sp] b_urban_ncp; // slope of urban effect 
  vector[n_sp] b_method_ncp; // slope of method effect 
  vector[n_sp] b_um_ncp;
  
  //vector[n_sp] b_urban;
  //vector[n_sp] b_method;
  //vector[n_sp] b_um;
  
	}
	
transformed parameters{
  //vector[n_sp] a_sp = mu_a_sp + sigma_a_sp*a_sp_ncp; 
  vector[n_sp] b_urban = mu_b_urban_sp + sigma_b_urban_sp*b_urban_ncp; 
  vector[n_sp] b_method = mu_b_method_sp + sigma_b_method_sp*b_method_ncp; 
  vector[n_sp] b_um = mu_b_um_sp + sigma_b_um_sp*b_um_ncp;
  
  vector[N] yhat;
  
  for(i in 1:N){    
    yhat[i] = a_sp[sp[i]] + // indexed with species
		          b_urban[sp[i]] * urban[i] + 
		          b_method[sp[i]] * method[i] +
		          b_um[sp[i]] *  inter_urbanmethod[i];
	      }
  
}

model {
  
	target+= normal_lpdf(b_urban_ncp | 0,1);
	target+= normal_lpdf(b_method_ncp | 0,1);
	target+= normal_lpdf(b_um_ncp | 0,1);
	
	target+= normal_lpdf(a_sp | mu_a_sp,sigma_a_sp);
	//target+= normal_lpdf(b_urban | mu_b_urban_sp, sigma_b_urban_sp);
	//target+= normal_lpdf(b_method | mu_b_method_sp, sigma_b_method_sp);
	//target+= normal_lpdf(b_um | mu_b_um_sp, sigma_b_um_sp);
	//target += normal_lpdf(to_vector(a_sp) | 0, 300);
	target += normal_lpdf(to_vector(b_urban) | 0, 60);
	target += normal_lpdf(to_vector(b_method) | 0, 60);
	target += normal_lpdf(to_vector(b_um) | 0, 60);
	     
        target+= normal_lpdf(mu_a_sp | 350,75);
	      target+= normal_lpdf(sigma_a_sp | 0,50);
        
        /*target+= normal_lpdf(mu_b_urban_sp | 0,50);
	      target+= normal_lpdf(sigma_b_urban_sp | 0,30);
        
        target+= normal_lpdf(mu_b_method_sp | 0,50);
	      target+= normal_lpdf(sigma_b_method_sp | 0,30);
	      
	      target+= normal_lpdf(mu_b_um_sp | 0,50);
	      target+= normal_lpdf(sigma_b_um_sp | 0,30);*/
        
        target+= normal_lpdf(sigma_y | 0,75);
  

	   target += normal_lpdf(y | yhat, sigma_y);   
	//y ~ normal(yhat, sigma_y);


}

/*generated quantities{
   real y_ppc[N];
   vector[n_sp] b_urban_post = mu_b_urban_sp + sigma_b_urban_sp*b_urban_ncp; 
   vector[n_sp] b_method_post = mu_b_method_sp + sigma_b_method_sp*b_method_ncp; 
   vector[n_sp] b_um_post = mu_b_um_sp + sigma_b_um_sp*b_um_ncp; 
   
   for (n in 1:N)
      y_ppc[n] = a_sp[sp[n]] + 
		b_urban[sp[n]] * urban[n] +
		b_method[sp[n]] * method[n] +
		b_um[sp[n]] *  inter_urbanmethod[n];
    for (n in 1:N)
      y_ppc[n] = normal_rng(y_ppc[n], sigma_y);

}*/