# mixture_models
This toy example comes from https://www.youtube.com/watch?v=oHWNexYPFTY&t=1247s. The seminar speaker presents the model but does not explain it, so here is my attempt at doing so. Despite it is a very simple example, the toy example captures all the steps required to fit mixture models in STAN and can be adapted for zero inflated Poisson regression or other custom mixture models. I also show how to make posterior predictve simulations and calculate model likelihood for WAIC and model comparison.

## Background

We want to model the effect of a cat being present in a garden on the number of bird songs. We have data regarding the number of songs, but only data on whether we *observe* the cat rather than if it is actually *present*, since cats are notoriously sneaky and could be hidden from our sight. Nevertheless, despite we only have data on whether the cat is *observed*, we can still estimate the probability that that cat is *present*! This can be used to evaluate how good we are in spotting the cat, which is very useful in case we are working with data real data regarding species presence or absence. The way to model this is with a mixture model. Mixture models combine mulitple likelihood functions, which in our example describe two possible scenarios: 1) the number of songs when the cat is not seen and not present, 2) the number of songs when the cat is not seen but actually present. This combined likelihood is simply the sum of the likelihood of each scenario weighted by the probability of that scenario. The way to implement this in STAN (lines 31 to 40) relies on conditioning on the observed data, such that the "mixture nature" of the model is only evident when we have to calculate the likelihood when the cat is not present. STAN user's guide on mixture models is great: https://mc-stan.org/docs/stan-users-guide/finite-mixtures.html. 

## Code

I first simulate whether the cat is present, then I simulate the data that could be gathered from an experiment: the number of songs and whether the cat is observed. Then I compile and fit the STAN model misterious_mixture.stan to see if I can recover the correct simulation parameter values.  I use the generated quantities block to make posterior predicitve simulations. For every parameter combination given by the MCMC sampler, the generated quantities block simulates the observed data (number of songs and whether cat is observed), and also whether the cat is present. The fact that we can use the model to simulate an unknown quantity as cat presence is useful for more complex models such as Hidden Markov Models, where our main interest is to known a hidden individual state which we cannot observe directly. In the generated quantities block I also store the likelihood of every observation. This is used to calculate the log pointwise posterior probability (lppd), the counterpart of the frequentist likelihood that includes all the parameter distribution rather than just the one associated with the maximum likelihood. From the lppd, we can calculate the WAIC. I use the custom function of the loo package to do so because the lppd includes a sum of probabilities: this is difficult to calculate because we work with log probabilities. Coincidently, the numerically stable way to calculate the lppd includes the same custom function used in the STAN mixture model (log_sum_exp()) to marginalize over multiple likelihood functions.
