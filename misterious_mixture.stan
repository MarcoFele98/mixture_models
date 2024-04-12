data {
  int<lower = 0> number_obs; // number of datapoints
  int<lower = 0, upper = 1> is_observed_cat[number_obs]; // boolean of wheather or not cat is observed
  int<lower = 0> songs[number_obs]; // number of songs heard
}

parameters {
  real<lower = 0, upper = 1> p_cat_present; // probability of cat being present
  real<lower = 0, upper = 1> p_cat_observed; // probability of observing cat if the cat is present
  
  real<lower = 0> song_rate_cat; // rate of songs when cat is present. 
  real<lower = 0> song_rate_nocat; // rate of songs when cat is not present. 
}

model {
  // prios (the sampling statement "~" does not include priors in likelihood calculation)
  p_cat_present ~ beta(1.1, 1.1);
  p_cat_observed ~ beta(1.1, 1.1);
  
  song_rate_cat ~ exponential(0.05);
  song_rate_nocat ~ exponential(0.05);
  
  // Model
  for(i in 1:number_obs) {
    if(is_observed_cat[i] == 1) { // only one scenario if the cat is observed
      target += log(p_cat_present) + // the cat is present
        log(p_cat_observed) + // the cat is observed 
        poisson_lpmf(songs[i] | song_rate_cat); // songs observed
    } // is_observed_cat == 1
    else { // two things could happen if the cat is not observed, and you have to marginalize (here is the mixture model part)
      target += log_sum_exp( // to marginalize just sum the two likelihoods that make the mixture weighted by their probabilities
        // 1)
        log(p_cat_present) + // the cat is present
        log1m(p_cat_observed) + // the cat is not observed - log1m is computationlly stable log(1 - probability)
        poisson_lpmf(songs[i] | song_rate_cat), // songs observed
        // 2)
        log1m(p_cat_present) + // the cat is not present
        // + log(1) = 0 -> the cat is not observed -> P(cat not observed | cat is not present) == 1
        poisson_lpmf(songs[i] | song_rate_nocat)); // songs observed
    } // is_observed_cat == 0
  } // i in 1:number_obs
}

generated quantities { // Generate quantities of interest from model

  // Posterior predictive simulations
  int<lower = 0, upper = 1> predictions_is_present_cat[number_obs]; // All variables declared here will be added to the output
  int<lower = 0, upper = 1> predictions_is_observed_cat[number_obs];
  int<lower = 0> predictions_songs[number_obs];

  for(i in 1:number_obs) {
    int is_present_cat = bernoulli_rng(p_cat_present); // this will not go in output bc defined inside {}
    if(is_present_cat == 1) {
      predictions_is_present_cat[i] = 1; // We can simulate "latent" (hidden - unknown stuff) as well!!
      predictions_is_observed_cat[i] = bernoulli_rng(p_cat_observed);
      predictions_songs[i] =  poisson_rng(song_rate_cat);
    }
    else {
      predictions_is_present_cat[i] = 0; // We can simulate "latent" (hidden - unknown stuff) as well!!
      predictions_is_observed_cat[i] = 0;
      predictions_songs[i] = poisson_rng(song_rate_nocat);
    }
  }
  
  // Calculate likelihood for WAIC
  vector<upper = 0>[number_obs] data_log_lk;
  
  for(i in 1:number_obs) {
    if(is_observed_cat[i] == 1) { // if the cat is observed only one distribution for likelihood
    data_log_lk[i] = log(p_cat_present) + // the cat is present
    log(p_cat_observed) + // the cat is observed 
    poisson_lpmf(songs[i] | song_rate_cat); // songs observed
    } // is_observed_cat == 1
    else { // if the cat is not observed two distributions for likelihood
    data_log_lk[i] = log_sum_exp(
    log(p_cat_present) + // the cat is present
    log1m(p_cat_observed) + // the cat is not observed
    poisson_lpmf(songs[i] | song_rate_cat), // songs observed
    log1m(p_cat_present) + // the cat is not present
    poisson_lpmf(songs[i] | song_rate_nocat) // songs observed
    ); 
    } // is_observed_cat == 0
  } // i in 1:number_obs
}
