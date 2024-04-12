# Mixture models in STAN

library(cmdstanr)
library(ggplot2)
library(bayesplot) # for diagnostics and posterior
library(loo) # for waic()

# Parameters ----
parameters <- list(
  number_obs = 100,
  p_cat_present = 0.25,
  p_cat_observed = 0.75,
  song_rate_nocat = 15,
  song_rate_cat = 10)

parameter_names <- c("p_cat_present", 
                     "p_cat_observed", 
                     "song_rate_cat", 
                     "song_rate_nocat")

# Simulate ----

# Hidden (we would not know this in a real scenario)
is_present_cat <- rbinom(parameters$number_obs, 1, p = parameters$p_cat_present)

# Observed (data)
is_observed_cat <- is_present_cat * rbinom(parameters$number_obs, 1, p = parameters$p_cat_observed)
songs <- rpois(parameters$number_obs, parameters$song_rate_cat) * is_present_cat + 
  rpois(parameters$number_obs, parameters$song_rate_nocat) * !is_present_cat

# Stan ----

# Priors
plot(seq(0, 1, l = 100), # my domain specific knowledge about cats in gardens tells me that the cat is going to be present and I will be able to observe it at least once, and that it will not be present and I will not be able to observe it at least once
     dbeta(seq(0, 1, l = 100), 1.1, 1.1))

plot(seq(0, 30, l = 100), # never heard birds make more than 30 songs, this is a sensible prior
     dexp(seq(0, 30, l = 100), 0.05))
# If priors bother you, do a sensitivity analysis: hopefully they will be washed away by the data and have little to not impact on inference

# Data
data_stan <- list(number_obs = parameters$number_obs, 
             is_observed_cat = is_observed_cat,
             songs = songs)

# Compile 
model <- cmdstan_model(stan_file = "mixture models/misterious_mixture.stan") 

# Sample 
fit_model <- model$sample(data_stan, 
                          iter_warmup = 200,
                          iter_sampling = 2000,
                          chains = 4)

model_summary <- fit_model$summary()[fit_model$summary()$variable %in% parameter_names, ]

draws_parameters <- fit_model$draws(variables = parameter_names,
                                    format = "df") 

# Plot
ggplot(draws_parameters) +
  geom_histogram(aes(p_cat_observed, 
                     color = .chain, 
                     group = .chain), 
                 position = "identity",
                 fill = NA) +
  geom_vline(aes(xintercept = parameters$p_cat_observed))

ggplot(draws_parameters) +
  geom_histogram(aes(song_rate_nocat, 
                     color = .chain, 
                     group = .chain), 
                 position = "identity",
                 fill = NA) +
  geom_vline(aes(xintercept = parameters$song_rate_nocat))

ggplot(draws_parameters) +
  geom_histogram(aes(song_rate_cat, 
                     color = .chain, 
                     group = .chain), 
                 position = "identity",
                 fill = NA) +
  geom_vline(aes(xintercept = parameters$song_rate_cat))

# Diagnostics plots
mcmc_pairs(draws_parameters, pars = parameter_names)

mcmc_scatter(draws_parameters, pars = c("p_cat_observed", 
                                        "song_rate_nocat"))

mcmc_trace(draws_parameters, window = c(600, 800) 
           )

mcmc_neff(neff_ratio(fit_model, 
                     pars = parameter_names))

# Calculate WAIC ----

draws_log_lk <- fit_model$draws(variables = paste0("data_log_lk[", 1:parameters$number_obs, "]"))
waic(draws_log_lk)

