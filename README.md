# mixture_models
This toy example comes from https://www.youtube.com/watch?v=oHWNexYPFTY&t=1247s. The speaker presents the model but does not explain it (sneaky), so here is my attempt at doing so. Despite it is a very simple example, it captures all the steps required to fit mixture models in STAN and can be adapted for zero inflated Poisson regression or other custom mixture models. I also show how to make posterior predictve simulations and calculate model likelihood for WAIC and model comparison.

## Background

We want to model the effect of a cat being present in a garden on the number of bird songs. We have data regarding the number of songs, but only data on whether we *observe* the cat rather than if the cat is actually present (cat are notoriously sneaky and could be present but not seen). The way to model this is with a mixture model. Mixture models combine mulitple likelihood functions, which in our example describe two possible scenarios: 1) the cat is not seen and not present, 2) the cat is not seen but present. STAN user's guide on mixture models is great: https://mc-stan.org/docs/stan-users-guide/finite-mixtures.html.

## Code

I first simulate whether the cat is present, then I simulate the data gathered from an experiment: the number of songs and whether the cat is observed. Then I compile and fit the STAN model misterious_mixture.stan. I use the generated quantities block to make posterior predicitve simulations. For every sample possible parameter combination given by the MCMC smapler, I simulate the observed data (number of songs and whether cat obserevd), and also whether the cat is truly present or not. The fact that we can use the model to simulate unknonw quantities (and not only our data) is very cool and useful for more complex models such as Hidden Markov Models, where our main interest is to known a hidden individual state which we cannot observe directly. In the generated quantities block I also store the likelihood of every obserevation. This can be used to calculate the log pointwise posterior probabaility (lppd), the counterpart of the frequentist model likelihood that uses all the posterior parameter distribution rather than just the one associated with the maximum likelihood. From the lppd, we can calculate the WAIC. I use the custom function of the loo package to do so becuase the lppd includes a sum of probabilities: this is difficult to calculate becuase we usually work with log probabilities. Coincidently, the numerically stable way to calculate lppd would be the same as the custom function used in the mixture model to amrginalize over the multiple likelihood functions.
