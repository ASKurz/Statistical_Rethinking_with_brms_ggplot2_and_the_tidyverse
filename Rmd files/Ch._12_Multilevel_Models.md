---
title: "Ch. 12 Multilevel Models"
author: "A Solomon Kurz"
date: "2018-04-21"
output:
  html_document:
    code_folding: show
    keep_md: TRUE
---

## 12.1. Example: Multilevel tadpoles

Let's get the `reedfrogs` data from rethinking.


```r
library(rethinking)
data(reedfrogs)
d <- reedfrogs
```

Detach rethinking and load brms.


```r
rm(reedfrogs)
detach(package:rethinking, unload = T)
library(brms)
```

Go ahead and acquaint yourself with the `reedfrogs`.


```r
library(tidyverse)
d %>%
  glimpse()
```

```
## Observations: 48
## Variables: 5
## $ density  <int> 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 1...
## $ pred     <fctr> no, no, no, no, no, no, no, no, pred, pred, pred, pr...
## $ size     <fctr> big, big, big, big, small, small, small, small, big,...
## $ surv     <int> 9, 10, 7, 10, 9, 9, 10, 9, 4, 9, 7, 6, 7, 5, 9, 9, 24...
## $ propsurv <dbl> 0.90, 1.00, 0.70, 1.00, 0.90, 0.90, 1.00, 0.90, 0.40,...
```

Making the `tank` cluster variable is easy.


```r
d <- 
  d %>%
  mutate(tank = 1:nrow(d))
```

Here's the un-pooled model in which each `tank` gets its own intercept.


```r
b12.1 <- 
  brm(data = d, family = binomial,
      surv | trials(density) ~ 0 + factor(tank),
      prior = c(set_prior("normal(0, 5)", class = "b")),
      chains = 4, iter = 2000, warmup = 500, cores = 4)
```

You specify the corresponding multilevel model like this.


```r
b12.2 <- 
  brm(data = d, family = binomial,
      surv | trials(density) ~ 1 + (1 | tank),
      prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                set_prior("cauchy(0, 1)", class = "sd")),
      chains = 4, iter = 4000, warmup = 1000, cores = 4)
```

The syntax for the varying effects follows the [lme4 style](https://cran.r-project.org/web/packages/brms/vignettes/brms_overview.pdf), `( [varying predictor(s)] | [grouping variable(s) )`. In this case `(1 | tank)` indicates only the intercept, `1`, varies by `tank`. The extent to which they vary is controlled by the prior `set_prior("cauchy(0, 1)", class = "sd")`, which is parameterized in the standard deviation metric.

Instead of computing the information criteria for each model, saving the results as objects and then placing those objects in `compare_ic()`, we can also just but both fit objects in `waic()` or `loo()`.


```r
waic(b12.1, b12.2)
```

```
##                 WAIC   SE
## b12.1         201.41 9.46
## b12.2         200.74 7.20
## b12.1 - b12.2   0.67 4.58
```

```r
loo(b12.1, b12.2)
```

```
## Warning: Found 36 observations with a pareto_k > 0.7 in model 'b12.1'.
## With this many problematic observations, it may be more appropriate to use
## 'kfold' with argument 'K = 10' to perform 10-fold cross-validation rather
## than LOO.
```

```
## Warning: Found 41 observations with a pareto_k > 0.7 in model 'b12.2'.
## With this many problematic observations, it may be more appropriate to use
## 'kfold' with argument 'K = 10' to perform 10-fold cross-validation rather
## than LOO.
```

```
##                LOOIC    SE
## b12.1         231.59 10.65
## b12.2         229.17  8.35
## b12.1 - b12.2   2.42  6.98
```

Note those "pareto_k > 0.7" warnings. We can follow the advice and use the `kfold()` function, instead. We'll also go ahead and specify `K = 10`, as recommended. But beware, this takes a few minutes.


```r
kf <- kfold(b12.1, b12.2, 
            K = 10, cores = 4)
```


```r
kf
```

```
##               KFOLDIC    SE
## b12.1          324.15 13.08
## b12.2          266.25 13.36
## b12.1 - b12.2   57.90  8.63
```

The $K$-fold cross-validation difference of 58, with a standard error around 9, suggests that model `b12.2` is the clear favorite relative to `b12.1`. For more on the `kfold()` function, see the [brms reference manual](https://cran.r-project.org/web/packages/brms/brms.pdf).

But here's our prep work for Figure 12.1


```r
post <- posterior_samples(b12.2)

invlogit <- function(x){1/(1+exp(-x))}

postMdn <- 
  coef(b12.2, robust = T) %>% data.frame() %>%
  add_column(tank = d$tank,
             density = d$density,
             propsurv = d$propsurv) %>%
  mutate(postMdn = invlogit(tank.Estimate.Intercept))
```

Recall that we can use Gelman and Hill's (2007) `invlogit()` function in place of the `logistic()` function in rethinking.

For kicks and giggles, let's use a [FiveThirtyEight-like theme](https://github.com/alex23lemm/theme_fivethirtyeight) for our plots. An easy way to do so is with help from the [ggthemes package](https://cran.r-project.org/web/packages/ggthemes/index.html).


```r
# install.packages("ggthemes", dependencies = T) 

library(ggthemes) 
```

Finally, our ggplot2 code to reproduce Figure 12.1


```r
postMdn %>%
  ggplot(aes(x = tank, y = postMdn)) +
  theme_fivethirtyeight() +
  geom_hline(yintercept = invlogit(median(post$b_Intercept)), linetype = 2, size = 1/4) +
  geom_vline(xintercept = c(16.5, 32.5), size = 1/4) +
  geom_point(shape = 1) +
  geom_point(aes(y = propsurv), color = "orange2") +
  coord_cartesian(ylim = c(0, 1)) +
  scale_x_continuous(breaks = c(1, 16, 32, 48)) +
  labs(title = "Proportion of survivors in each tank",
       subtitle = "The empirical proportions are in orange while the\nmodel-implied proportions are the black circles.\nThe dashed line is the model-implied average survival proportion.") +
  annotate("text", x = c(8, 16 + 8, 32 + 8), y = 0, 
           label = c("small tanks", "medium tanks", "large tanks")) +
  theme(panel.grid = element_blank())
```

![](Ch._12_Multilevel_Models_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

Here is our version of Figure 12.2.a. 


```r
tibble(x = c(-3, 4)) %>%
  
  ggplot(aes(x = x)) + 
  theme_fivethirtyeight() +
  mapply(function(mean, sd) {
    stat_function(fun = dnorm, 
                  args = list(mean = mean, sd = sd), 
                  alpha = .2, 
                  color = "orange2")
  }, 
  # Enter means and standard deviations here
  mean = post[1:100, 1],
  sd = post[1:100, 2]
  ) +
  labs(title = "Survival in log-odds") +
  scale_y_continuous(NULL, breaks = NULL)
```

![](Ch._12_Multilevel_Models_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

I got the idea to nest `stat_function()` within `mapply()` from [shadow's answer to this Stack Overflow question](http://stackoverflow.com/questions/27009641/plot-multiple-normal-curves-in-same-plot).

Anyway, here's the code for Figure 12.2.b.


```r
ggplot(data = post, 
       aes(x = invlogit(rnorm(nrow(post), mean = post[, 1], sd = post[, 2])))) +
  theme_fivethirtyeight() +
  geom_density(size = 0, fill = "orange2") +
  labs(title = "Probability of survival") +
  scale_y_continuous(NULL, breaks = NULL)
```

![](Ch._12_Multilevel_Models_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

Note how we sampled 12,000 imaginary `tanks` rather than McElreath's 8,000. This is because we had 12,000 HMC iterations (i.e., `nrow(post)`).

The `aes()` code, above, was a bit much. To get a sense of how it worked, consider this:


```r
rnorm(1, mean = post[, 1], sd = post[, 2]) %>% 
  invlogit()
```

```
## [1] 0.9897544
```

First, we took one random draw from a normal distribution with a mean of the first row in `post[, 1]` and a standard deviation of the value from the first row in `post[, 2]`, and passed it through the `invlogit()` function. By replacing the `1` `nrow(post)`, we do this `nrow(post)` times (i.e., 12,000). So our orange density is the summary of that process.

##### Overthinking: Prior for variance components.

Yep, you can use the exponential distribution for your priors in brms. Here it is for model `b12.2`.


```r
b12.2.e <- 
  brm(data = d, family = binomial,
      surv | trials(density) ~ 1 + (1 | tank),
      prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                set_prior("exponential(1)", class = "sd")),
      chains = 4, iter = 2000, warmup = 500, cores = 4)
```

The model summary:


```r
print(b12.2.e)
```

```
##  Family: binomial 
##   Links: mu = logit 
## Formula: surv | trials(density) ~ 1 + (1 | tank) 
##    Data: d (Number of observations: 48) 
## Samples: 4 chains, each with iter = 2000; warmup = 500; thin = 1; 
##          total post-warmup samples = 6000
##     ICs: LOO = NA; WAIC = NA; R2 = NA
##  
## Group-Level Effects: 
## ~tank (Number of levels: 48) 
##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## sd(Intercept)     1.61      0.21     1.25     2.07       1542 1.00
## 
## Population-Level Effects: 
##           Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## Intercept     1.30      0.25     0.82     1.80       1054 1.00
## 
## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
## is a crude measure of effective sample size, and Rhat is the potential 
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

If you're curious how the exponential prior compares to the posterior, you might just plot.


```r
ggplot(data = tibble(x = seq(from = 0, to = 4, by = .01)), 
       aes(x = x)) +
  theme_fivethirtyeight()+
  geom_ribbon(aes(ymin = 0, ymax = dexp(x, rate = 1)),  # the prior
              fill = "orange2", alpha = 1/3) +
  geom_density(data = posterior_samples(b12.2.e),       # the posterior
               aes(x = sd_tank__Intercept), 
               size = 0, fill = "orange2") +
  geom_vline(xintercept = posterior_samples(b12.2.e)[, 2] %>% median(),
             color = "blue", linetype = 2) +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = c(0, 3.5)) +
  labs(title = "Bonus prior/posterior plot\n for `sd_tank__Intercept`",
       subtitle = "The prior is the semitransparent ramp in the\nbackground. The posterior is the solid orange\nmound. The dashed line is the posterior median.")
```

![](Ch._12_Multilevel_Models_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

## 12.2. Varying effects and the underfitting/overfitting trade-off

### 12.2.2. Assign values to the parameters.


```r
a      <- 1.4
sigma  <- 1.5
nponds <- 60
ni     <- rep(c(5, 10, 25, 35), each = 15) %>% as.integer()

set.seed(10579595) # To make results reproducible
dsim <- 
  tibble(pond = 1:nponds,
         ni = ni,
         true_a = rnorm(nponds, mean = a, sd = sigma))
```

### 12.2.3. Sumulate survivors.


```r
set.seed(10579595) # To make results reproducible
dsim <-
  dsim %>%
  mutate(si = rbinom(nponds, prob = invlogit(true_a), size = ni)) %>%
  mutate(p_nopool = si/ni) 

dsim %>% 
  glimpse()
```

```
## Observations: 60
## Variables: 5
## $ pond     <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16...
## $ ni       <int> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 10, 10, ...
## $ true_a   <dbl> 3.1085100, 3.6798855, 2.6097976, 4.2842340, 3.2304515...
## $ si       <int> 4, 5, 4, 5, 5, 4, 4, 2, 4, 2, 5, 4, 2, 4, 4, 10, 7, 1...
## $ p_nopool <dbl> 0.8, 1.0, 0.8, 1.0, 1.0, 0.8, 0.8, 0.4, 0.8, 0.4, 1.0...
```

### 12.2.5. Compute the partial-pooling estimates.

Our one-chain model in brms.


```r
b12.3 <- 
  brm(data = dsim, family = binomial,
      si | trials(ni) ~ 1 + (1 | pond),
      prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                set_prior("cauchy(0, 1)", class = "sd")),
      chains = 1, iter = 10000, warmup = 1000, cores = 1)
```

```
## 
## SAMPLING FOR MODEL 'binomial brms-model' NOW (CHAIN 1).
## 
## Gradient evaluation took 7.1e-05 seconds
## 1000 transitions using 10 leapfrog steps per transition would take 0.71 seconds.
## Adjust your expectations accordingly!
## 
## 
## Iteration:    1 / 10000 [  0%]  (Warmup)
## Iteration: 1000 / 10000 [ 10%]  (Warmup)
## Iteration: 1001 / 10000 [ 10%]  (Sampling)
## Iteration: 2000 / 10000 [ 20%]  (Sampling)
## Iteration: 3000 / 10000 [ 30%]  (Sampling)
## Iteration: 4000 / 10000 [ 40%]  (Sampling)
## Iteration: 5000 / 10000 [ 50%]  (Sampling)
## Iteration: 6000 / 10000 [ 60%]  (Sampling)
## Iteration: 7000 / 10000 [ 70%]  (Sampling)
## Iteration: 8000 / 10000 [ 80%]  (Sampling)
## Iteration: 9000 / 10000 [ 90%]  (Sampling)
## Iteration: 10000 / 10000 [100%]  (Sampling)
## 
##  Elapsed Time: 0.872771 seconds (Warm-up)
##                5.70663 seconds (Sampling)
##                6.57941 seconds (Total)
```

```r
print(b12.3)
```

```
##  Family: binomial 
##   Links: mu = logit 
## Formula: si | trials(ni) ~ 1 + (1 | pond) 
##    Data: dsim (Number of observations: 60) 
## Samples: 1 chains, each with iter = 10000; warmup = 1000; thin = 1; 
##          total post-warmup samples = 9000
##     ICs: LOO = NA; WAIC = NA; R2 = NA
##  
## Group-Level Effects: 
## ~pond (Number of levels: 60) 
##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## sd(Intercept)     1.34      0.18     1.02     1.73       2920 1.00
## 
## Population-Level Effects: 
##           Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## Intercept     1.29      0.20     0.90     1.70       3098 1.00
## 
## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
## is a crude measure of effective sample size, and Rhat is the potential 
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

I'm not aware that you can use McElreath's `depth = 2` trick in brms for `summary()` or `print()`. But can get that information with the `coef()` function. 


```r
coef(b12.3)$pond[c(1:2, 59:60), , ] %>% 
  round(digits = 2)
```

```
##    Estimate Est.Error 2.5%ile 97.5%ile
## 1      1.47      0.88   -0.13     3.28
## 2      2.34      1.01    0.54     4.50
## 59     1.59      0.42    0.80     2.47
## 60     2.59      0.59    1.54     3.87
```

Here we get ready for the diagnostic plot, Figure 12.3.


```r
dsim %>% 
  glimpse()
```

```
## Observations: 60
## Variables: 5
## $ pond     <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16...
## $ ni       <int> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 10, 10, ...
## $ true_a   <dbl> 3.1085100, 3.6798855, 2.6097976, 4.2842340, 3.2304515...
## $ si       <int> 4, 5, 4, 5, 5, 4, 4, 2, 4, 2, 5, 4, 2, 4, 4, 10, 7, 1...
## $ p_nopool <dbl> 0.8, 1.0, 0.8, 1.0, 1.0, 0.8, 0.8, 0.4, 0.8, 0.4, 1.0...
```


```r
p_partpool <- 
  coef(b12.3) %>% 
  data.frame() %>%  # as_tibble() didn't work well, for this.
  select(pond.Estimate.Intercept) %>%
  mutate(pond.Estimate.Intercept = invlogit(pond.Estimate.Intercept)) %>%
  pull()

dsim <- 
  dsim %>%
  mutate(p_true = invlogit(true_a)) %>%
  mutate(nopool_error = abs(p_nopool - p_true)) %>%
  mutate(partpool_error = abs(p_partpool - p_true))

dsim %>% 
  glimpse()
```

```
## Observations: 60
## Variables: 8
## $ pond           <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, ...
## $ ni             <int> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 10...
## $ true_a         <dbl> 3.1085100, 3.6798855, 2.6097976, 4.2842340, 3.2...
## $ si             <int> 4, 5, 4, 5, 5, 4, 4, 2, 4, 2, 5, 4, 2, 4, 4, 10...
## $ p_nopool       <dbl> 0.8, 1.0, 0.8, 1.0, 1.0, 0.8, 0.8, 0.4, 0.8, 0....
## $ p_true         <dbl> 0.9572424, 0.9753948, 0.9314895, 0.9864032, 0.9...
## $ nopool_error   <dbl> 0.15724241, 0.02460518, 0.13148948, 0.01359676,...
## $ partpool_error <dbl> 0.1445969494, 0.0632212877, 0.1207325431, 0.074...
```

Here is our code for Figure 12.3. The extra data processing for `dfline` is how we get the values necessary for the horizontal summary lines.


```r
dfline <- 
  dsim %>%
  select(ni, nopool_error:partpool_error) %>%
  gather(key, value, -ni) %>%
  group_by(key, ni) %>%
  summarise(mean_error = mean(value)) %>%
  mutate(x = c(1, 16, 31, 46),
         xend = c(15, 30, 45, 60))
  
ggplot(data = dsim, aes(x = pond)) +
  theme_fivethirtyeight() +
  geom_vline(xintercept = c(15.5, 30.5, 45.4), 
             color = "white", size = 2/3) +
  geom_point(aes(y = nopool_error), color = "orange2") +
  geom_point(aes(y = partpool_error), shape = 1) +
  geom_segment(data = dfline, 
               aes(x = x, xend = xend, 
                   y = mean_error, yend = mean_error),
               color = rep(c("orange2", "black"), each = 4),
               linetype = rep(1:2, each = 4)) +
  labs(y = "absolute error",
       title = "Estimate error by model type",
       subtitle = "The horizontal axis displays pond number. The vertical\naxis measures the absolute error in the predicted proportion\nof survivors, compared to the true value used in the simulation.\nThe higher the point, the worse the estimate. No-pooling shown\nin orange. Partial pooling shown in black. The orange and\ndashed black lines show the average error for each kind of\nestimate, across each initial density of tadpoles (pond size).\nSmaller ponds produce more error, but the partial pooling\nestimates are better on average, especially in smaller ponds.") +
  scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50, 60)) +
  annotate("text", x = c(15 - 7.5, 30 - 7.5, 45 - 7.5, 60 - 7.5), y = .45, 
           label = c("tiny (5)", "small (10)", "medium (25)", "large (35)")) +
  theme(panel.grid = element_blank())
```

![](Ch._12_Multilevel_Models_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

If you wanted to quantify the difference in simple summaries, you might do something like this:


```r
dsim %>%
  select(ni, nopool_error:partpool_error) %>%
  gather(key, value, -ni) %>%
  group_by(key) %>%
  summarise(mean_error   = mean(value) %>% round(digits = 3),
            median_error = median(value) %>% round(digits = 3))
```

```
## # A tibble: 2 x 3
##   key            mean_error median_error
##   <chr>               <dbl>        <dbl>
## 1 nopool_error       0.0730       0.0480
## 2 partpool_error     0.0630       0.0430
```

### 12.3.1. Multilevel chimpanzees.

Our two identical Gaussians in a tidy tibble.


```r
set.seed(241)
two_gaussians <- 
  tibble(y1 = rnorm(n = 1e4, mean = 10, sd = 1),
         y2 = 10 + rnorm(n = 1e4, mean = 0, sd = 1))
```

Let's follow McElreath's advice to make sure they are same by superimposing the density of one on the other.


```r
two_gaussians %>%
  
  ggplot() +
  theme_fivethirtyeight() +
  geom_density(aes(x = y1), 
               size = 0, fill = "orange1", alpha = 1/3) +
  geom_density(aes(x = y2), 
               size = 0, fill = "orange4", alpha = 1/3) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = "My simulated Gaussians")
```

![](Ch._12_Multilevel_Models_files/figure-html/unnamed-chunk-28-1.png)<!-- -->

Yep, those Gaussians look about the same.

Let's get the `chimpanzees` data from rethinking.


```r
library(rethinking)
data(chimpanzees)
d <- chimpanzees
```

Detach rethinking and reload brms.


```r
rm(chimpanzees)
detach(package:rethinking, unload = T)
library(brms)
```

Our brms model with varying intercepts for `actor` but not `block`.


```r
b12.4 <- 
  brm(data = d, family = binomial,
      pulled_left ~ 1 + prosoc_left + prosoc_left:condition + (1 | actor),
      prior = c(set_prior("normal(0, 10)", class = "Intercept"),
                set_prior("normal(0, 10)", class = "b"),
                set_prior("cauchy(0, 1)", class = "sd")),
      chains = 4, iter = 5000, warmup = 1000, cores = 4,
      control = list(adapt_delta = 0.95))
```

The initial solutions came with a few divergent transitions. Increasing `adapt_delta` to `.95` solved the problem. You can also solve the problem with more strongly regularizing priors such as `normal(0, 2)` on the intercept and slope parameters (see https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations). Consider trying both methods and comparing the results. They're similar. 

Here we add the `actor`-level deviations to the fixed intercept, the grand mean.


```r
post <- posterior_samples(b12.4)

post %>%
  select(`r_actor[1,Intercept]`:`r_actor[7,Intercept]`) %>%
  gather() %>%
  # This is how we add the grand mean to the actor-level deviations
  mutate(value = value + post$b_Intercept) %>% 
  group_by(key) %>%
  summarise(mean = mean(value) %>% round(digits = 2))
```

```
## # A tibble: 7 x 2
##   key                    mean
##   <chr>                 <dbl>
## 1 r_actor[1,Intercept] -0.710
## 2 r_actor[2,Intercept]  4.59 
## 3 r_actor[3,Intercept] -1.02 
## 4 r_actor[4,Intercept] -1.02 
## 5 r_actor[5,Intercept] -0.710
## 6 r_actor[6,Intercept]  0.230
## 7 r_actor[7,Intercept]  1.76
```

Here's another way to get at the same information, this time using `coef()` and a little formatting help from the `tidyverse::str_c()` function. Just for kicks, we'll throw in the 95% intervals, too.


```r
coef(b12.4)$actor[ , c(1, 3:4), 1] %>%
  as_tibble() %>%
  round(digits = 2) %>%
  # Here we put the credible intervals in an APA-6-style format
  mutate(`95% CIs` = str_c("[", `2.5%ile`, ", ", `97.5%ile`, "]")) %>%
  mutate(actor = str_c("chimp #", 1:7)) %>%
  rename(mean = Estimate) %>%
  select(actor, mean, `95% CIs`)
```

```
## # A tibble: 7 x 3
##   actor      mean `95% CIs`     
##   <chr>     <dbl> <chr>         
## 1 chimp #1 -0.710 [-1.24, -0.18]
## 2 chimp #2  4.59  [2.56, 8.43]  
## 3 chimp #3 -1.02  [-1.58, -0.48]
## 4 chimp #4 -1.02  [-1.58, -0.48]
## 5 chimp #5 -0.710 [-1.26, -0.19]
## 6 chimp #6  0.230 [-0.29, 0.75] 
## 7 chimp #7  1.76  [1.06, 2.53]
```

If you prefer the posterior median to the mean, just add a `robust = T` argument inside the `coef()` function.

### 12.3.2. Two types of cluster.

Our brms model with varying intercepts for both `actor` and `block`.


```r
b12.5 <- 
  brm(data = d, family = binomial,
      pulled_left ~ 1 + prosoc_left + prosoc_left:condition + 
        (1 | actor) + (1 | block),
      prior = c(set_prior("normal(0, 10)", class = "Intercept"),
                set_prior("normal(0, 10)", class = "b"),
                set_prior("cauchy(0, 1)", class = "sd")),
      chains = 4, iter = 6000, warmup = 1000, cores = 4,
      control = list(adapt_delta = 0.99))
```

Again with the divergent transitions issue. Increasing `adapt_delta` to `.99` worked fine. Again, we can look at the primary coefficients with `print()`, but need to use the `coef()` function to get those `depth = 2` estimates.


```r
print(b12.5)
```

```
##  Family: binomial 
##   Links: mu = logit 
## Formula: pulled_left ~ 1 + prosoc_left + prosoc_left:condition + (1 | actor) + (1 | block) 
##    Data: d (Number of observations: 504) 
## Samples: 4 chains, each with iter = 6000; warmup = 1000; thin = 1; 
##          total post-warmup samples = 20000
##     ICs: LOO = NA; WAIC = NA; R2 = NA
##  
## Group-Level Effects: 
## ~actor (Number of levels: 7) 
##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## sd(Intercept)     2.26      0.93     1.12     4.57       5773 1.00
## 
## ~block (Number of levels: 6) 
##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## sd(Intercept)     0.22      0.18     0.01     0.68       8284 1.00
## 
## Population-Level Effects: 
##                       Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## Intercept                 0.46      0.97    -1.43     2.55       4179 1.00
## prosoc_left               0.83      0.26     0.32     1.35      17258 1.00
## prosoc_left:condition    -0.14      0.30    -0.73     0.45      16514 1.00
## 
## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
## is a crude measure of effective sample size, and Rhat is the potential 
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

```r
coef(b12.5)$actor[, , "Intercept"] %>% 
  round(digits = 2)
```

```
##   Estimate Est.Error 2.5%ile 97.5%ile
## 1    -0.71      0.30   -1.30    -0.14
## 2     4.63      1.58    2.54     8.62
## 3    -1.02      0.30   -1.62    -0.44
## 4    -1.02      0.31   -1.63    -0.44
## 5    -0.72      0.29   -1.30    -0.13
## 6     0.23      0.29   -0.34     0.81
## 7     1.77      0.40    1.03     2.61
```

```r
coef(b12.5)$block[, , "Intercept"] %>% 
  round(digits = 2)
```

```
##   Estimate Est.Error 2.5%ile 97.5%ile
## 1     0.28      0.99   -1.61     2.38
## 2     0.50      0.98   -1.38     2.58
## 3     0.51      0.98   -1.39     2.57
## 4     0.46      0.98   -1.41     2.55
## 5     0.43      0.98   -1.45     2.49
## 6     0.57      0.98   -1.33     2.65
```

We might make the coefficient plot in Figure 12.4.a. like this:


```r
library(bayesplot)
color_scheme_set("orange")

stanplot(b12.5, pars = c("^r_", "^b_", "^sd_")) +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_text(hjust = 0))
```

![](Ch._12_Multilevel_Models_files/figure-html/unnamed-chunk-36-1.png)<!-- -->

You don't always have to explicitly call bayesplot with `library()`, but doing so allowed us to alter the default color scheme.

Once we get the posterior samples, it's easy to compare the random variances as in Figure 12.4.b.


```r
posterior_samples(b12.5) %>%
  
  ggplot(aes(x = sd_actor__Intercept)) +
  theme_fivethirtyeight() +
  geom_density(size = 0, fill = "orange1", alpha = 3/4) +
  geom_density(aes(x = sd_block__Intercept), 
               size = 0, fill = "orange4", alpha = 3/4)  +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = c(0, 4)) +
  labs(title = expression(sigma)) +
  annotate("text", x = 2/3, y = 2, label = "block", color = "orange4") +
  annotate("text", x = 2, y = 3/4, label = "actor", color = "orange1")
```

![](Ch._12_Multilevel_Models_files/figure-html/unnamed-chunk-37-1.png)<!-- -->

We might compare our models by their PSIS-LOO values.


```r
l.b12.4 <- loo(b12.4, cores = 4)
```

```
## Using the maximum response value as the number of trials.
```

```r
l.b12.5 <- loo(b12.5, cores = 4)
```

```
## Using the maximum response value as the number of trials.
```

```r
compare_ic(l.b12.4, l.b12.5)
```

```
##                LOOIC    SE
## b12.4         531.44 19.47
## b12.5         532.79 19.68
## b12.4 - b12.5  -1.35  1.72
```

And you can get the LOO version of the `p_waic`, the `p_loo`, like so.


```r
l.b12.4
```

```
## 
## Computed from 16000 by 504 log-likelihood matrix
## 
##          Estimate   SE
## elpd_loo   -265.7  9.7
## p_loo         8.1  0.4
## looic       531.4 19.5
## ------
## Monte Carlo SE of elpd_loo is 0.0.
## 
## All Pareto k estimates are good (k < 0.5).
## See help('pareto-k-diagnostic') for details.
```

```r
l.b12.5
```

```
## 
## Computed from 20000 by 504 log-likelihood matrix
## 
##          Estimate   SE
## elpd_loo   -266.4  9.8
## p_loo        10.4  0.5
## looic       532.8 19.7
## ------
## Monte Carlo SE of elpd_loo is 0.0.
## 
## All Pareto k estimates are good (k < 0.5).
## See help('pareto-k-diagnostic') for details.
```

And if you peek at the structure of the loo objects, you'll see you can call the `p_loo` values directly with something like `l.b12.5$ estimates["p_loo",]`.

The results are quite similar to those in the text.

Anyways, the two models yield nearly-equivalent information criteria values. Yet recall what McElreath wrote: "There is nothing to gain here by selecting either model. The comparison of the two models tells a richer story" (p. 367).

## 12.4. Multilevel posterior predictions

### 12.4.2 Posterior prediction for new clusters. 

It'll take a bit of prep work to make Figure 12.5. First, let's use `posterior_summary()` to survey at the posterior.


```r
posterior_summary(b12.4)
```

```
##                             Estimate Est.Error      2.5%ile     97.5%ile
## b_Intercept                0.4338808 0.9481819   -1.3430613    2.4251020
## b_prosoc_left              0.8172876 0.2580356    0.3155837    1.3206135
## b_prosoc_left:condition   -0.1274315 0.2919477   -0.7000943    0.4432054
## sd_actor__Intercept        2.2526737 0.9167157    1.1241921    4.6232926
## r_actor[1,Intercept]      -1.1428554 0.9646670   -3.1835356    0.6977285
## r_actor[2,Intercept]       4.1515888 1.5983383    1.7708463    8.0243244
## r_actor[3,Intercept]      -1.4504129 0.9668190   -3.5226384    0.3572580
## r_actor[4,Intercept]      -1.4518991 0.9672759   -3.5062100    0.3869243
## r_actor[5,Intercept]      -1.1481262 0.9649792   -3.1926106    0.6779769
## r_actor[6,Intercept]      -0.2050011 0.9624747   -2.2514881    1.6263234
## r_actor[7,Intercept]       1.3250156 0.9908587   -0.7422503    3.2345595
## lp__                    -282.9130692 2.8070885 -289.2315420 -278.3884987
```


```r
post <- posterior_samples(b12.4)

postAverageActor <-
  # Here we use the linear regression formula to get the probabilities for the 4 conditions
  tibble(C00 = invlogit(post[, 1]),
         C10 = invlogit(post[, 1] + post[, 2]),
         C01 = invlogit(post[, 1]),
         C11 = invlogit(post[, 1] + post[, 2] + post[, 3])) %>%
  # Putting the data in the long format and grouping by condition (i.e., key)
  gather() %>%
  group_by(key) %>%
  # Here we get the summary values for the plot
  summarise(M  = mean(value),
            LL = quantile(value, probs = .1),
            UL = quantile(value, probs = .9)) %>%
  mutate(Condition = c(1, 3, 2, 4)) %>%
  arrange(Condition)

postAverageActor
```

```
## # A tibble: 4 x 5
##   key       M    LL    UL Condition
##   <chr> <dbl> <dbl> <dbl>     <dbl>
## 1 C00   0.589 0.339 0.826      1.00
## 2 C10   0.744 0.536 0.917      2.00
## 3 C01   0.589 0.339 0.826      3.00
## 4 C11   0.722 0.504 0.906      4.00
```

Figure 12.5.a.


```r
postAverageActor %>%
  
  ggplot(aes(x = Condition, y = M)) +
  theme_fivethirtyeight() +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orange1") +
  geom_line(color = "blue") +
  scale_x_continuous(labels = c("0/0", "1/0", "0/1", "1/1")) +
  labs(y = "proportion pulled left",
       title = "Average actor",
       subtitle = "Condition specified by\nprosocial_left/condition") +
  coord_cartesian(ylim = c(0, 1))
```

![](Ch._12_Multilevel_Models_files/figure-html/unnamed-chunk-42-1.png)<!-- -->

Here's the necessary data wrangling for Figure 12.5.b.


```r
set.seed(6177024)
ran_ef <-
  tibble(actor = rnorm(1000, 0, post$sd_actor__Intercept))

# Here are the random effects
ran_ef <-
  bind_rows(ran_ef, ran_ef, ran_ef, ran_ef) %>%
  gather() %>%
  rename(random_effect = value) %>%
  select(random_effect)

# Here are the fixed effects (i.e., the population parameters)
fix_ef <-
  tibble(C00 = post[1:1000, 1],
         C10 = post[1:1000, 1] + post[1:1000, 2],
         C01 = post[1:1000, 1],
         C11 = post[1:1000, 1] + post[1:1000, 2] + post[1:1000, 3]) %>%
  gather() %>%
  rename(condition = key, fixed_effect = value)

# Here we combine them
ran_and_fix_ef <-
  bind_cols(ran_ef, fix_ef) %>%
  mutate(intercept = fixed_effect + random_effect) %>%
  mutate(prob = invlogit(intercept))

# To simplify things, we'll reduce them to summaries
marginal_effects <-
  ran_and_fix_ef %>%
  group_by(condition) %>%
  summarise(M  = mean(prob),
            LL = quantile(prob, probs = .1),
            UL = quantile(prob, probs = .9)) %>%
  mutate(Condition = c(1, 3, 2, 4))
```

Figure 12.5.b.


```r
marginal_effects %>%
  
  ggplot(aes(x = Condition, y = M)) +
  theme_fivethirtyeight() +
  geom_ribbon(aes(ymin = LL, ymax = UL), fill = "orange1") +
  geom_line(color = "blue") +
  scale_x_continuous(labels = c("0/0", "1/0", "0/1", "1/1")) +
  labs(y = "proportion pulled left",
       title = "Marginal\nof actor") +
  coord_cartesian(ylim = c(0, 1))
```

![](Ch._12_Multilevel_Models_files/figure-html/unnamed-chunk-44-1.png)<!-- -->

Figure 12.5.c. just takes a tiny bit of data wrangling. 


```r
ran_and_fix_ef %>%
  mutate(condition = factor(condition, levels = c("C00", "C10", "C01", "C11"))) %>%
  mutate(iter = rep(1:1000, times = 4)) %>%
  filter(iter %in% c(1:50)) %>%
  
  ggplot(aes(x = condition, y = prob, group = iter)) +
  theme_fivethirtyeight() +
  geom_line(alpha = 1/2, color = "orange3") +
  scale_x_discrete(labels = c("0/0", "1/0", "0/1", "1/1")) +
  labs(y = "proportion pulled left",
       title = "50 simulated\nactors") +
  coord_cartesian(ylim = c(0, 1))
```

![](Ch._12_Multilevel_Models_files/figure-html/unnamed-chunk-45-1.png)<!-- -->

### 12.4.3. Focus and multilevel prediction.

First, let's get that `Kline` data.


```r
# prep data
library(rethinking)
data(Kline)
k <- Kline
```

Switching packages, once again.


```r
detach(package:rethinking, unload = T)
library(brms)
rm(Kline)
```

With brms, we don't actually need to make the logpop or society variables. We're ready to fit the multilevel `Kline` model with the data in hand.


```r
b12.6 <- 
  brm(data = k, family = poisson,
      total_tools ~ 0 + intercept + log(population) + 
        (1 | culture),
      prior = c(set_prior("normal(0, 10)", class = "b", coef = "intercept"),
                set_prior("normal(0, 1)", class = "b"),
                set_prior("cauchy(0, 1)", class = "sd")),
      chains = 3, iter = 4000, warmup = 1000, cores = 3)
```

Note how we used the special `0 + intercept` syntax rather than using the default Intercept. This is because our predictor variable was not mean centered. For more info, see [here](https://github.com/paul-buerkner/brms/issues/114). Though we used the `0 + intercept` syntax for the fixed effect, it was not necessary for the random effect. Both ways work.

Here is the data-processing work for our variant of Figure 12.6.


```r
nd <- 
  tibble(population = seq(from = 1000, to = 300000, by = 5000),
         # To "simulate counterfactual societies, using the hyper-parameters" (p. 383), we'll plug a new island into the `culture` variable
         culture = "My_island") 

p12.6 <-
  predict(b12.6,
          # This allows us to simulate values for our counterfactual island, "My_island"
          allow_new_levels = T,
          # Here we explicitly tell brms we want to include the group-level effects
          re_formula = ~ (1 | culture),
          # From the brms manual, this uses the "(multivariate) normal distribution implied by the group-level standard deviations and correlations", which appears to be what McElreath did in the text.
          sample_new_levels = "gaussian",
          newdata = nd,
          probs = c(.015, .055, .165, .835, .945, .985)) %>%
  as_tibble() %>%
  bind_cols(nd)

p12.6 %>%  
  glimpse()
```

```
## Observations: 60
## Variables: 10
## $ Estimate   <dbl> 19.70822, 31.14944, 36.33389, 40.30067, 43.32856, 4...
## $ Est.Error  <dbl> 9.996754, 14.050138, 16.006741, 18.257357, 19.56248...
## $ `1.5%ile`  <dbl> 5, 10, 13, 14, 15, 16, 17, 17, 18, 19, 19, 19, 20, ...
## $ `5.5%ile`  <dbl> 8, 14, 17, 19, 21, 22, 23, 24, 25, 26, 27, 27, 27, ...
## $ `16.5%ile` <dbl> 11, 20, 23, 26, 28, 30, 31, 32, 33, 34, 35, 36, 37,...
## $ `83.5%ile` <dbl> 27.000, 41.165, 48.000, 53.000, 57.000, 60.000, 63....
## $ `94.5%ile` <dbl> 35.000, 52.055, 61.000, 68.000, 72.000, 77.000, 81....
## $ `98.5%ile` <dbl> 47.000, 69.015, 80.000, 89.000, 97.015, 100.000, 10...
## $ population <dbl> 1000, 6000, 11000, 16000, 21000, 26000, 31000, 3600...
## $ culture    <chr> "My_island", "My_island", "My_island", "My_island",...
```

For a detailed discussion on this way of using `brms::predict()`, see [Andrew MacDonald’s great blogpost on this very figure](http://thestudyofthehousehold.com/2018/02/13/2018-02-13-easily-made-fitted-and-predicted-values-made-easy/).

Here's our version of the figure:


```r
p12.6 %>%
  ggplot(aes(x = log(population), y = Estimate)) +
  theme_fivethirtyeight() +
  geom_ribbon(aes(ymin = `1.5%ile`, ymax = `98.5%ile`), fill = "orange2", alpha = 1/3) +
  geom_ribbon(aes(ymin = `5.5%ile`, ymax = `94.5%ile`), fill = "orange2", alpha = 1/3) +
  geom_ribbon(aes(ymin = `16.5%ile`, ymax = `83.5%ile`), fill = "orange2", alpha = 1/3) +
  coord_cartesian(ylim = range(k$total_tools)) +
  geom_line(color = "orange4") +
  geom_text(data = k, aes(y = total_tools, label = culture), 
            size = 2.25, color = "blue") +
  labs(subtitle = "Total tools as a function of log(population)")
```

![](Ch._12_Multilevel_Models_files/figure-html/unnamed-chunk-50-1.png)<!-- -->



Note. The analyses in this document were done with:

* R           3.4.4
* RStudio     1.1.442
* rmarkdown   1.9
* rstan       2.17.3
* rethinking  1.59
* brms        2.2.0
* tidyverse   1.2.1 
* ggthemes    3.4.0
* bayesplot   1.5.0

## References

Gelman, A., & Hill, J. (2007). *Data analysis using regression and multilevel/hierarchical models.* New York, NY, US: Cambridge University Press. 
McElreath, R. (2016). *Statistical rethinking: A Bayesian course with examples in R and Stan.* Chapman & Hall/CRC Press.
