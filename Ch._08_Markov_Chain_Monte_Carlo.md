---
title: "Ch. 8 Markov Chain Monte Carlo"
author: "A Solomon Kurz"
date: "2018-03-27"
output:
  html_document:
    code_folding: show
    keep_md: TRUE
---

## 8.1. Good King Markov and His island kingdom.

In this version of the code, we've added `set.seed()`, which helps make the exact results reproducible. If you're new to setting seeds, play around with different values.


```r
set.seed(103)

num_weeks <- 1e5
positions <- rep(0, num_weeks)
current   <- 10
for (i in 1:num_weeks) {
  # record current position
  positions[i] <- current
  # flip coin to generate proposal
  proposal <- current + sample(c(-1, 1), size = 1)
  # now make sure he loops around the archipelago
  if (proposal < 1) proposal <- 10
  if (proposal > 10) proposal <- 1
  # move?
  prob_move <- proposal/current
  current <- ifelse(runif(1) < prob_move, proposal, current)
}
```

In this document, we'll borrow a theme, `theme_ipsum()`, from the [hrbrthemes package](https://cran.r-project.org/web/packages/hrbrthemes/index.html).


```r
# install.packages("hrbrthemes", dependencies = T)
library(hrbrthemes)
```

Figure 8.2.a.


```r
library(tidyverse)

tibble(week = 1:1e5,
       island = positions) %>%
  filter(week < 101) %>%

  ggplot(aes(x = week, y = island)) +
  geom_point(shape = 1) +
  scale_x_continuous(breaks = seq(from = 0, to = 100, by = 20)) +
  scale_y_continuous(breaks = seq(from = 0, to = 10, by = 2)) +
  labs(title = "Behold: The Metropolis algorithm in action!",
       subtitle = "The dots show the king's path over the first 100 weeks.") +
  theme_ipsum()
```

![](Ch._08_Markov_Chain_Monte_Carlo_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Figure 8.2.b.


```r
tibble(week = 1:1e5,
       island = positions) %>%
  mutate(island = factor(island)) %>%

  ggplot(aes(x = island)) +
  geom_bar() +
  labs(title = "Old Metropolis shines in the long run.",
       subtitle = "Sure enough, the time the king spent on each island was\nproportional to its population size.") +
  theme_ipsum()
```

![](Ch._08_Markov_Chain_Monte_Carlo_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## 8.3. Easy HMC: ~~map2stan~~ `brm()`

Here we load the `rugged` data.


```r
library(rethinking)
data(rugged)
d <- rugged
```

Switching from rethinking to brms.


```r
detach(package:rethinking)
library(brms)
rm(rugged)
```

It takes just a sec to do a little data manipulation. 


```r
d <- 
  d %>%
  mutate(log_gdp = log(rgdppc_2000))

dd <-
  d %>%
  filter(complete.cases(rgdppc_2000))
```

In the context of this chapter, it doesn't make sense to translate McElreath's m8.1 `map()` code to `brm()` code. Below, we'll just go directly to the `brm()` variant of his `m8.1stan`.

### 8.3.1. Preparation. 

You don't need to do the data processing McElreath does on pages 248 and 249. If you wanted to, however, here's how you might do it within the tidyverse.


```r
dd.trim <-
  dd %>%
  select(log_gdp, rugged, cont_africa)

str(dd.trim)
```

### 8.3.2. Estimation.

Finally, we get to work that sweet HMC.


```r
b8.1 <-
  brm(data = dd, family = gaussian,
      log_gdp ~ 1 + rugged + cont_africa + rugged:cont_africa,
      prior = c(set_prior("normal(0, 100)", class = "Intercept"),
                set_prior("normal(0, 10)", class = "b"),
                set_prior("cauchy(0, 2)", class = "sigma")))
```

The posterior:


```r
print(b8.1)
```

```
##  Family: gaussian 
##   Links: mu = identity; sigma = identity 
## Formula: log_gdp ~ 1 + rugged + cont_africa + rugged:cont_africa 
##    Data: dd (Number of observations: 170) 
## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1; 
##          total post-warmup samples = 4000
##     ICs: LOO = NA; WAIC = NA; R2 = NA
##  
## Population-Level Effects: 
##                    Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## Intercept              9.22      0.14     8.96     9.50       2570 1.00
## rugged                -0.20      0.08    -0.36    -0.04       2495 1.00
## cont_africa           -1.95      0.23    -2.40    -1.50       2228 1.00
## rugged:cont_africa     0.39      0.13     0.14     0.65       2111 1.00
## 
## Family Specific Parameters: 
##       Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## sigma     0.95      0.05     0.86     1.06       4000 1.00
## 
## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
## is a crude measure of effective sample size, and Rhat is the potential 
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

### 8.3.3. Sampling again, in parallel.

Here we add `chains = 4, cores = 4`.


```r
b8.1_4chains_4cores <-
  brm(data = dd, family = gaussian,
      log_gdp ~ 1 + rugged + cont_africa + rugged:cont_africa,
      prior = c(set_prior("normal(0, 100)", class = "Intercept"),
                set_prior("normal(0, 10)", class = "b"),
                set_prior("cauchy(0, 2)", class = "sigma")),
      chains = 4, cores = 4)
```

This model sampled so fast that it really didn't matter if we sampled in parallel or not. It will for others.


```r
print(b8.1_4chains_4cores)
```

```
##  Family: gaussian 
##   Links: mu = identity; sigma = identity 
## Formula: log_gdp ~ 1 + rugged + cont_africa + rugged:cont_africa 
##    Data: dd (Number of observations: 170) 
## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1; 
##          total post-warmup samples = 4000
##     ICs: LOO = NA; WAIC = NA; R2 = NA
##  
## Population-Level Effects: 
##                    Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## Intercept              9.22      0.14     8.95     9.50       2943 1.00
## rugged                -0.20      0.08    -0.36    -0.05       2800 1.00
## cont_africa           -1.95      0.23    -2.39    -1.49       2263 1.00
## rugged:cont_africa     0.39      0.13     0.12     0.66       2309 1.00
## 
## Family Specific Parameters: 
##       Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## sigma     0.95      0.05     0.86     1.06       4000 1.00
## 
## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
## is a crude measure of effective sample size, and Rhat is the potential 
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

### 8.3.4. Visualization.

Unlike the way rethinking's `extract.samples()` yields a list, brms's `posterior_samples()` returns a data frame.


```r
post <- posterior_samples(b8.1)
str(post)
```

```
## 'data.frame':	4000 obs. of  6 variables:
##  $ b_Intercept         : num  9.24 9.11 9.18 9.29 9.35 ...
##  $ b_rugged            : num  -0.146 -0.205 -0.211 -0.225 -0.26 ...
##  $ b_cont_africa       : num  -2.03 -1.95 -1.92 -2.1 -1.88 ...
##  $ b_rugged:cont_africa: num  0.385 0.43 0.505 0.403 0.391 ...
##  $ sigma               : num  0.86 1.035 0.876 0.887 0.987 ...
##  $ lp__                : num  -248 -249 -248 -247 -248 ...
```

As with McElreath's rethinking, brms allows users to put the `post` data frame or the brmsfit object directly in `pairs()`.


```r
pairs(b8.1)
```

![](Ch._08_Markov_Chain_Monte_Carlo_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

Another nice way to customize your pairs plot is with the [GGally package](https://cran.r-project.org/web/packages/GGally/index.html).


```r
# install.packages("GGally", dependencies = T)
library(GGally)
```


```r
post %>%
  select(b_Intercept:sigma) %>%
  
  ggpairs()
```

The above code returned an error: "variables: "x" have non standard format: "b_rugged:cont_africa". Please rename the columns or make a new column." GGally didn't like the way brms named one of our parameters, `b_rugged:cont_africa`. That's easy enough to fix.


```r
my_pairs_plot <-
  post %>%
  rename(b_rugged_x_cont_africa = `b_rugged:cont_africa`) %>%
  select(b_Intercept:sigma) %>%
  
  ggpairs()

my_pairs_plot
```

![](Ch._08_Markov_Chain_Monte_Carlo_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

Since GGally returns a ggplot object, you can customize it as you please.


```r
my_pairs_plot +
  labs(subtitle = "My custom pairs plot") +
  theme_ipsum()
```

![](Ch._08_Markov_Chain_Monte_Carlo_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

For more ideas on customizing a GGally pairs plot, go [here](http://ggobi.github.io/ggally/#columns_and_mapping).

### 8.3.5. Using the samples.

If you want information criteria as a part of your brms model summary, just add `loo = T` and/or `waic = T` in the `summary()` function. 


```r
summary(b8.1, loo = T, waic = T)
```

```
##  Family: gaussian 
##   Links: mu = identity; sigma = identity 
## Formula: log_gdp ~ 1 + rugged + cont_africa + rugged:cont_africa 
##    Data: dd (Number of observations: 170) 
## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1; 
##          total post-warmup samples = 4000
##     ICs: LOO = 469.15; WAIC = 469.02; R2 = NA
##  
## Population-Level Effects: 
##                    Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## Intercept              9.22      0.14     8.96     9.50       2570 1.00
## rugged                -0.20      0.08    -0.36    -0.04       2495 1.00
## cont_africa           -1.95      0.23    -2.40    -1.50       2228 1.00
## rugged:cont_africa     0.39      0.13     0.14     0.65       2111 1.00
## 
## Family Specific Parameters: 
##       Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## sigma     0.95      0.05     0.86     1.06       4000 1.00
## 
## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
## is a crude measure of effective sample size, and Rhat is the potential 
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

But beware: the information criteria haven’t taken long to estimate in the models we’ve fit thus far in the text. As your models become more complex and as your data get larger, the time their information criteria will take to compute will increase. So, if you have a complex multilevel model with a large data set, you might be better off computing the information criteria separately from the model summary.

### 8.3.6. Checking the chain.

Using `plot()` for a `brm()` fit returns both density and trace lots for the parameters.


```r
plot(b8.1)
```

![](Ch._08_Markov_Chain_Monte_Carlo_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

The bayesplot package allows a little more control. Here, we use [bayesplot's](https://cran.r-project.org/web/packages/bayesplot/index.html) `mcmc_trace()` to show only trace plots with our custom theme. Note that `mcmc_trace()` works with data frames, not brmfit objects. There's a further complication. Recall how we made `post` (i.e., `post <- posterior_samples(b8.1)`). Our `post` data frame carries no information on chains. To retain that information, we'll need to add an `add_chain = T` argument to our `posterior_samples()` function.


```r
library(bayesplot)

post <- posterior_samples(b8.1, add_chain = T)

mcmc_trace(post[, c(1:5, 7)], # We need to include column 7 because that contains the chain info 
           facet_args = list(ncol = 3), 
           size = .15) +
  labs(title = "My custom trace plots") +
  scale_color_ipsum() +
  theme_ipsum() +
  theme(legend.position = c(.95, .2))
```

![](Ch._08_Markov_Chain_Monte_Carlo_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

The bayesplot package offers a variety of diagnostic plots. Here we make autocorrelation plots for all model parameters, one for each HMC chain.


```r
mcmc_acf(post, 
         pars = c("b_Intercept", "b_rugged", "b_cont_africa", "b_rugged:cont_africa", "sigma"),
         lags = 5) +
  scale_color_ipsum() +
  theme_ipsum()
```

![](Ch._08_Markov_Chain_Monte_Carlo_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

That's just what we like to see--nice L-shaped autocorrelation plots. Those are the kinds of shapes you'd expect when you have reasonably large effective samples. Anyway...

##### Overthinking: Raw Stan model code.

The `stancode()` function works in brms much like it does in rethinking.


```r
stancode(b8.1)
```

```
## // generated with brms 2.1.9
## functions { 
## } 
## data { 
##   int<lower=1> N;  // total number of observations 
##   vector[N] Y;  // response variable 
##   int<lower=1> K;  // number of population-level effects 
##   matrix[N, K] X;  // population-level design matrix 
##   int prior_only;  // should the likelihood be ignored? 
## } 
## transformed data { 
##   int Kc = K - 1; 
##   matrix[N, K - 1] Xc;  // centered version of X 
##   vector[K - 1] means_X;  // column means of X before centering 
##   for (i in 2:K) { 
##     means_X[i - 1] = mean(X[, i]); 
##     Xc[, i - 1] = X[, i] - means_X[i - 1]; 
##   } 
## } 
## parameters { 
##   vector[Kc] b;  // population-level effects 
##   real temp_Intercept;  // temporary intercept 
##   real<lower=0> sigma;  // residual SD 
## } 
## transformed parameters { 
## } 
## model { 
##   vector[N] mu = Xc * b + temp_Intercept; 
##   // priors including all constants 
##   target += normal_lpdf(b | 0, 10); 
##   target += normal_lpdf(temp_Intercept | 0, 100); 
##   target += cauchy_lpdf(sigma | 0, 2)
##     - 1 * cauchy_lccdf(0 | 0, 2); 
##   // likelihood including all constants 
##   if (!prior_only) { 
##     target += normal_lpdf(Y | mu, sigma); 
##   } 
## } 
## generated quantities { 
##   // actual population-level intercept 
##   real b_Intercept = temp_Intercept - dot_product(means_X, b); 
## }
```

You can also get that information with `b8.1$model` or `b8.1$fit@stanmodel`.

## 8.4. Care and feeding of your Markov chain.

### 8.4.3. Taming a wild chain.

As with rethinking, brms can take data in the form of a list. Recall however, that in order to specify starting values, you need to specify a list of lists with an `inits` argument, rather than with `start`, as in rethinking.


```r
b8.2 <-
  brm(data = list(y = c(-1, 1)), 
      family = gaussian,
      y ~ 1,
      prior = c(set_prior("uniform(-1e10, 1e10)", class = "Intercept"),
                set_prior("uniform(0, 1e10)", class = "sigma")),
      inits = list(list(Intercept = 0, sigma = 1),
                   list(Intercept = 0, sigma = 1)),
      chains = 2, iter = 4000, warmup = 1000)
```

Those were some silly flat priors. Here's the damage. 


```r
post <- posterior_samples(b8.2, add_chain = T)

mcmc_trace(post[, c(1:2, 4)],
           size = .25) +
  labs(title = "My version of Figure 8.5.a.",
       subtitle = "These trace plots do not look like the fuzzy caterpillars we usually hope for.") +
  scale_color_ipsum() +
  theme_ipsum() +
  theme(legend.position = c(.85, 1.5),
        legend.direction = "horizontal")
```

![](Ch._08_Markov_Chain_Monte_Carlo_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

Let's peek at the summary.


```r
print(b8.2)
```

```
## Warning: There were 411 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help.
## See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
```

```
##  Family: gaussian 
##   Links: mu = identity; sigma = identity 
## Formula: y ~ 1 
##    Data: list(y = c(-1, 1)) (Number of observations: 2) 
## Samples: 2 chains, each with iter = 4000; warmup = 1000; thin = 1; 
##          total post-warmup samples = 6000
##     ICs: LOO = NA; WAIC = NA; R2 = NA
##  
## Population-Level Effects: 
##              Estimate    Est.Error      l-95% CI     u-95% CI Eff.Sample
## Intercept -9381278.35 264223729.30 -747487598.08 536966887.33         47
##           Rhat
## Intercept 1.03
## 
## Family Specific Parameters: 
##           Estimate     Est.Error l-95% CI      u-95% CI Eff.Sample Rhat
## sigma 348895812.70 1049117124.27 75477.37 3613144907.13         47 1.04
## 
## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
## is a crude measure of effective sample size, and Rhat is the potential 
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

Holy smokes, those parameters are a mess! Plus we got a nasty warning message, too. Watch our reasonable priors save the day.


```r
b8.3 <-
  brm(data = list(y = c(-1, 1)), 
      family = gaussian,
      y ~ 1,
      prior = c(set_prior("normal(0, 10)", class = "Intercept"),
                set_prior("cauchy(0, 1)", class = "sigma")),
      inits = list(list(Intercept = 0, sigma = 1),
                   list(Intercept = 0, sigma = 1)),
      chains = 2, iter = 4000, warmup = 1000)
```


```r
print(b8.3)
```

```
##  Family: gaussian 
##   Links: mu = identity; sigma = identity 
## Formula: y ~ 1 
##    Data: list(y = c(-1, 1)) (Number of observations: 2) 
## Samples: 2 chains, each with iter = 4000; warmup = 1000; thin = 1; 
##          total post-warmup samples = 6000
##     ICs: LOO = NA; WAIC = NA; R2 = NA
##  
## Population-Level Effects: 
##           Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## Intercept     0.03      1.71    -3.07     3.22       1057 1.00
## 
## Family Specific Parameters: 
##       Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## sigma     1.99      2.20     0.60     6.66       1069 1.00
## 
## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
## is a crude measure of effective sample size, and Rhat is the potential 
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

As in the text, no more warning signs and no more silly estimates. The trace plots look great, too.


```r
post <- posterior_samples(b8.3, add_chain = T)

mcmc_trace(post[, c(1:2, 4)],
           size = .25) +
  labs(title = "My version of Figure 8.5.b",
       subtitle  = "Oh man. This looks so much better.") +
  scale_color_ipsum() +
  theme_ipsum() +
  theme(legend.position = c(.85, 1.5),
        legend.direction = "horizontal")
```

![](Ch._08_Markov_Chain_Monte_Carlo_files/figure-html/unnamed-chunk-29-1.png)<!-- -->

Our version of Figure 8.6.a.


```r
post %>%
  select(b_Intercept) %>%
  
  ggplot(aes(x = b_Intercept)) +
  stat_density(geom = "line") +
  geom_line(data = data.frame(x = seq(from = min(post$b_Intercept),
                                      to = max(post$b_Intercept),
                                      length.out = 50)),
            aes(x = x, y = dnorm(x = x, mean = 0, sd = 10)),
            color = ipsum_pal()(1), linetype = 2) +
  theme_ipsum()
```

![](Ch._08_Markov_Chain_Monte_Carlo_files/figure-html/unnamed-chunk-30-1.png)<!-- -->
  
And our version of Figure 8.6.b.
  

```r
post %>%
  select(sigma) %>%
  
  ggplot(aes(x = sigma)) +
  stat_density(geom = "line") +
  geom_line(data = data.frame(x = seq(from = 0,
                                      to = max(post$sigma),
                                      length.out = 50)),
            aes(x = x, y = dcauchy(x = x, location = 0, scale = 1)*2),
            color = ipsum_pal()(2)[2], linetype = 2) +
  coord_cartesian(xlim = c(0, 10)) +
  theme_ipsum()
```

![](Ch._08_Markov_Chain_Monte_Carlo_files/figure-html/unnamed-chunk-31-1.png)<!-- -->

### 8.4.4. Non-identifiable parameters.

I couldn't figure out how to specify this one in brms. For example, the code below didn't quite work. If you've got it, hit a brother up.


```r
y <- rnorm(100, mean = 0, sd = 1)

b8.4 <-
  brm(data = list(y = y), 
      family = gaussian,
      y ~ 1 + 1,
      prior = c(set_prior("uniform(-1e10, 1e10)", class = "Intercept"),
                set_prior("cauchy(0, 1)", class = "sigma")),
      inits = list(list(Intercept = 0, sigma = 1),
                   list(Intercept = 0, sigma = 1)),
      chains = 2, iter = 4000, warmup = 1000)

print(b8.4)
```

But anyways, the central message in the text, default to weakly-regularizing priors, holds for brms just as it does in rethinking. For more on the topic, see the [recommendations from the Stan team](https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations). If you want to dive deeper, check out [Dan Simpson's post on Gelman's blog](http://andrewgelman.com/2017/09/05/never-total-eclipse-prior/) and their [corresponding paper with Michael Betancourt](https://arxiv.org/abs/1708.07487).



Note. The analyses in this document were done with:

* R           3.4.4
* RStudio     1.1.442
* rmarkdown   1.9
* rethinking  1.59
* brms        2.1.9
* rstan       2.17.3
* tidyverse   1.2.1
* hrbrthemes  0.1.0
* GGally      1.3.0
* bayesplot   1.4.0

## Reference
McElreath, R. (2016). *Statistical rethinking: A Bayesian course with examples in R and Stan.* Chapman & Hall/CRC Press.

