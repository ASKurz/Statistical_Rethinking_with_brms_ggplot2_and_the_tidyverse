---
title: "Ch. 12 Multilevel Models"
author: "A Solomon Kurz"
date: "2018-03-25"
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
## b12.1         202.06 9.40
## b12.2         201.25 7.33
## b12.1 - b12.2   0.81 4.43
```

```r
loo(b12.1, b12.2)
```

```
##                LOOIC    SE
## b12.1         233.64 10.57
## b12.2         228.66  8.70
## b12.1 - b12.2   4.98  5.91
```



Unfortunately, this is one of those occasions in which the information criteria don't perfectly align. Gelman and his colleagues prefer the loo (e.g., https://arxiv.org/pdf/1507.04544.pdf).

`brms::loo()` also produced warning messages recommending we switch from the `loo()` function to the `kfold()` function with argument `K = 10`. This took several minutes and yielded a `b12.1 - b12.2` *K*-fold cross-validation difference of 59, with a standard error of about 14. For more on the `kfold()` function, see the [brms reference manual](https://cran.r-project.org/web/packages/brms/brms.pdf).

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

![](Ch._12_Multilevel_Models_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Here is our version of Figure 12.2.a. 


```r
tibble(x = c(-3, 4)) %>%
  
  ggplot(aes(x = x)) + 
  theme_fivethirtyeight() +
  mapply(function(mean, sd, size, alpha, color) {
    stat_function(fun = dnorm, 
                  args = list(mean = mean, sd = sd), 
                  alpha = alpha, 
                  color = color)
  }, 
  # Enter means, standard deviations, alpha, and color here
  mean = post[1:100, 1],
  sd = post[1:100, 2],
  alpha = .2,
  color = "orange2") +
  labs(title = "Survival in log-odds") +
  scale_y_continuous(NULL, breaks = NULL)
```

![](Ch._12_Multilevel_Models_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

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

![](Ch._12_Multilevel_Models_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

Note how we sampled 12,000 imaginary `tanks` rather than McElreath's 8,000. This is because we had 12,000 HMC iterations (i.e., `nrow(post)`).

The `aes()` code, above, was a bit much. To get a sense of how it worked, consider this:


```r
rnorm(1, mean = post[, 1], sd = post[, 2]) %>% 
  invlogit()
```

```
## [1] 0.9192972
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
## sd(Intercept)     1.61      0.21     1.25     2.08       1391 1.00
## 
## Population-Level Effects: 
##           Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## Intercept     1.31      0.25     0.83     1.81        982 1.00
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

![](Ch._12_Multilevel_Models_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

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
## Gradient evaluation took 6.8e-05 seconds
## 1000 transitions using 10 leapfrog steps per transition would take 0.68 seconds.
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
##  Elapsed Time: 0.834708 seconds (Warm-up)
##                5.73263 seconds (Sampling)
##                6.56734 seconds (Total)
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

I'm not aware that you can use McElreath's `depth = 2` trick in brms for `summary()` or `print()`. You can also get similar information by calling subcomponents in the brm object, such as with `b12.3$fit`. However, perhaps a better way is with the `tidy()` function in the [broom package](https://cran.r-project.org/web/packages/broom/index.html).


```r
library(broom)

tidy(b12.3) %>%
  mutate_if(is.numeric, round, digits = 2)  # This just rounds the values
```

```
##                    term estimate std.error   lower   upper
## 1           b_Intercept     1.29      0.20    0.97    1.63
## 2    sd_pond__Intercept     1.34      0.18    1.06    1.66
## 3   r_pond[1,Intercept]     0.18      0.88   -1.19    1.68
## 4   r_pond[2,Intercept]     1.06      1.00   -0.48    2.82
## 5   r_pond[3,Intercept]     0.17      0.88   -1.23    1.68
## 6   r_pond[4,Intercept]     1.06      1.03   -0.52    2.84
## 7   r_pond[5,Intercept]     1.07      1.04   -0.49    2.90
## 8   r_pond[6,Intercept]     0.18      0.88   -1.20    1.67
## 9   r_pond[7,Intercept]     0.18      0.90   -1.24    1.73
## 10  r_pond[8,Intercept]    -1.15      0.78   -2.44    0.13
## 11  r_pond[9,Intercept]     0.17      0.89   -1.23    1.68
## 12 r_pond[10,Intercept]    -1.14      0.81   -2.49    0.17
## 13 r_pond[11,Intercept]     1.06      1.02   -0.51    2.83
## 14 r_pond[12,Intercept]     0.19      0.89   -1.20    1.69
## 15 r_pond[13,Intercept]    -1.15      0.81   -2.48    0.15
## 16 r_pond[14,Intercept]     0.18      0.90   -1.22    1.75
## 17 r_pond[15,Intercept]     0.17      0.89   -1.26    1.69
## 18 r_pond[16,Intercept]     1.45      0.95    0.03    3.12
## 19 r_pond[17,Intercept]    -0.27      0.67   -1.34    0.87
## 20 r_pond[18,Intercept]     1.46      0.97    0.02    3.17
## 21 r_pond[19,Intercept]     0.72      0.78   -0.48    2.09
## 22 r_pond[20,Intercept]     0.18      0.73   -0.94    1.43
## 23 r_pond[21,Intercept]    -1.03      0.61   -2.04   -0.01
## 24 r_pond[22,Intercept]    -0.28      0.67   -1.35    0.87
## 25 r_pond[23,Intercept]    -0.67      0.63   -1.68    0.36
## 26 r_pond[24,Intercept]    -0.28      0.67   -1.35    0.86
## 27 r_pond[25,Intercept]     1.45      0.96   -0.01    3.12
## 28 r_pond[26,Intercept]     0.72      0.80   -0.52    2.12
## 29 r_pond[27,Intercept]     0.73      0.81   -0.53    2.12
## 30 r_pond[28,Intercept]     1.46      0.96    0.01    3.15
## 31 r_pond[29,Intercept]    -1.03      0.62   -2.05   -0.01
## 32 r_pond[30,Intercept]     1.45      0.97   -0.01    3.16
## 33 r_pond[31,Intercept]    -0.79      0.43   -1.50   -0.08
## 34 r_pond[32,Intercept]    -1.56      0.43   -2.28   -0.87
## 35 r_pond[33,Intercept]     0.67      0.59   -0.25    1.68
## 36 r_pond[34,Intercept]     1.44      0.72    0.33    2.67
## 37 r_pond[35,Intercept]     1.01      0.64    0.03    2.13
## 38 r_pond[36,Intercept]    -0.78      0.45   -1.50   -0.04
## 39 r_pond[37,Intercept]     2.03      0.86    0.75    3.61
## 40 r_pond[38,Intercept]     0.14      0.51   -0.67    1.01
## 41 r_pond[39,Intercept]     1.02      0.64    0.03    2.12
## 42 r_pond[40,Intercept]    -0.62      0.45   -1.36    0.11
## 43 r_pond[41,Intercept]    -2.39      0.48   -3.22   -1.62
## 44 r_pond[42,Intercept]    -0.94      0.44   -1.67   -0.21
## 45 r_pond[43,Intercept]    -0.78      0.43   -1.50   -0.06
## 46 r_pond[44,Intercept]    -0.94      0.43   -1.65   -0.23
## 47 r_pond[45,Intercept]     2.03      0.87    0.74    3.59
## 48 r_pond[46,Intercept]    -0.31      0.41   -0.97    0.37
## 49 r_pond[47,Intercept]     2.27      0.84    1.02    3.76
## 50 r_pond[48,Intercept]    -1.48      0.39   -2.12   -0.84
## 51 r_pond[49,Intercept]     1.00      0.56    0.12    1.97
## 52 r_pond[50,Intercept]    -0.31      0.41   -0.98    0.38
## 53 r_pond[51,Intercept]    -0.03      0.43   -0.71    0.70
## 54 r_pond[52,Intercept]    -1.47      0.38   -2.11   -0.85
## 55 r_pond[53,Intercept]    -1.04      0.39   -1.67   -0.39
## 56 r_pond[54,Intercept]     0.74      0.53   -0.08    1.65
## 57 r_pond[55,Intercept]    -2.06      0.41   -2.74   -1.41
## 58 r_pond[56,Intercept]     0.13      0.45   -0.59    0.90
## 59 r_pond[57,Intercept]    -2.47      0.44   -3.22   -1.77
## 60 r_pond[58,Intercept]    -1.81      0.39   -2.48   -1.19
## 61 r_pond[59,Intercept]     0.31      0.46   -0.44    1.08
## 62 r_pond[60,Intercept]     1.31      0.61    0.36    2.37
## 63                 lp__  -189.03      7.53 -201.87 -177.15
```

If you just want summaries of the pond-specific intercepts, you can also use `coef()`.


```r
coef(b12.3)
```

```
## $pond
## , , Intercept
## 
##      Estimate Est.Error    2.5%ile   97.5%ile
## 1   1.4672922 0.8762639 -0.1347969  3.2812885
## 2   2.3404674 1.0112221  0.5434239  4.4987127
## 3   1.4549361 0.8721497 -0.1431186  3.3016202
## 4   2.3420298 1.0369563  0.5558461  4.6019779
## 5   2.3505629 1.0499711  0.5503838  4.6897887
## 6   1.4680837 0.8847162 -0.1623285  3.3512209
## 7   1.4671371 0.8994973 -0.2018060  3.3658409
## 8   0.1369014 0.7698553 -1.4059991  1.6514291
## 9   1.4580097 0.8835502 -0.1815537  3.2940926
## 10  0.1408244 0.7935329 -1.4307165  1.7022065
## 11  2.3489988 1.0336145  0.4922234  4.5635452
## 12  1.4723908 0.8861492 -0.1711861  3.3014842
## 13  0.1378573 0.7990615 -1.4537455  1.6543724
## 14  1.4701382 0.8949266 -0.1588721  3.3776349
## 15  1.4537512 0.8882792 -0.2002074  3.3144590
## 16  2.7367969 0.9590715  1.0688931  4.8305879
## 17  1.0139601 0.6485036 -0.2066250  2.3724742
## 18  2.7452668 0.9751867  1.0659283  4.8908213
## 19  2.0032939 0.7793717  0.6091285  3.6550464
## 20  1.4685005 0.7158403  0.1664975  2.9637613
## 21  0.2504441 0.5932215 -0.8916364  1.4260286
## 22  1.0067354 0.6465546 -0.2373625  2.3682265
## 23  0.6172509 0.6070801 -0.5287594  1.8252020
## 24  1.0072485 0.6541481 -0.2253517  2.3626395
## 25  2.7399769 0.9646046  1.0627905  4.8101207
## 26  2.0075220 0.8018659  0.5784967  3.7052228
## 27  2.0123808 0.8110673  0.5811525  3.7362403
## 28  2.7475261 0.9661268  1.0475378  4.8684662
## 29  0.2597181 0.5930512 -0.9074329  1.4256130
## 30  2.7382663 0.9713898  1.0459889  4.9024719
## 31  0.4981178 0.3922885 -0.2607845  1.2648705
## 32 -0.2703048 0.3926988 -1.0532410  0.4871514
## 33  1.9563873 0.5658865  0.9418778  3.1599909
## 34  2.7232474 0.7147545  1.4796661  4.2811775
## 35  2.2970898 0.6216526  1.2096689  3.6482378
## 36  0.5042470 0.4043215 -0.2725094  1.3148516
## 37  3.3160802 0.8681395  1.8598014  5.2606545
## 38  1.4298663 0.4827802  0.5488416  2.4310254
## 39  2.3050636 0.6254272  1.1798365  3.6226488
## 40  0.6620651 0.4044288 -0.1218050  1.4679685
## 41 -1.1081841 0.4443530 -2.0173391 -0.2771433
## 42  0.3433068 0.3990890 -0.4297132  1.1368853
## 43  0.5049031 0.3932402 -0.2528883  1.2842238
## 44  0.3409455 0.3890724 -0.4174201  1.1372149
## 45  3.3182453 0.8775485  1.8491553  5.2702121
## 46  0.9764636 0.3684470  0.2857735  1.7294562
## 47  3.5518249 0.8428414  2.0914754  5.4270564
## 48 -0.1939611 0.3363536 -0.8783018  0.4585012
## 49  2.2809674 0.5410388  1.3216761  3.4429101
## 50  0.9743586 0.3662876  0.2824690  1.7235355
## 51  1.2586539 0.3933632  0.5237359  2.0480281
## 52 -0.1895613 0.3334211 -0.8328986  0.4625131
## 53  0.2489588 0.3383282 -0.4047799  0.9225035
## 54  2.0224323 0.5015390  1.1234890  3.0809187
## 55 -0.7744220 0.3564005 -1.4818463 -0.1012028
## 56  1.4188296 0.4164745  0.6427145  2.2868684
## 57 -1.1821746 0.3943089 -1.9880389 -0.4378672
## 58 -0.5293188 0.3409093 -1.2123709  0.1230481
## 59  1.5944583 0.4241797  0.8025414  2.4742048
## 60  2.5948102 0.5945618  1.5364971  3.8657174
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

Here is our code for Figure 12.3. The extra data processing for dfline is how we get the values necessary for the horizontal summary lines.


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
## 2 r_actor[2,Intercept]  4.61 
## 3 r_actor[3,Intercept] -1.02 
## 4 r_actor[4,Intercept] -1.02 
## 5 r_actor[5,Intercept] -0.720
## 6 r_actor[6,Intercept]  0.220
## 7 r_actor[7,Intercept]  1.75
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
## 1 chimp #1 -0.710 [-1.24, -0.2] 
## 2 chimp #2  4.61  [2.52, 8.56]  
## 3 chimp #3 -1.02  [-1.58, -0.48]
## 4 chimp #4 -1.02  [-1.58, -0.48]
## 5 chimp #5 -0.720 [-1.25, -0.2] 
## 6 chimp #6  0.220 [-0.3, 0.75]  
## 7 chimp #7  1.75  [1.04, 2.57]
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

Again with the divergent transitions issue. Increasing `adapt_delta` to `.99` worked fine. We'll use `tidy()` again to look at the model. In the `bind_cols()` function, we use `bayesplot::rhat()` and a little data processing to add the r_hat values. 


```r
tidy(b12.5) %>%
  bind_cols(rhat(b12.5) %>% as_tibble() %>% rename(r_hat = value)) %>%
  mutate_if(is.numeric, round, digits = 3)
```

```
##                       term estimate std.error    lower    upper r_hat
## 1              b_Intercept    0.452     0.977   -1.014    2.063 1.000
## 2            b_prosoc_left    0.830     0.261    0.409    1.266 1.000
## 3  b_prosoc_left:condition   -0.140     0.297   -0.624    0.350 1.000
## 4      sd_actor__Intercept    2.281     0.942    1.241    4.023 1.000
## 5      sd_block__Intercept    0.216     0.176    0.018    0.544 1.000
## 6     r_actor[1,Intercept]   -1.173     0.990   -2.812    0.312 1.000
## 7     r_actor[2,Intercept]    4.193     1.652    2.116    7.131 1.000
## 8     r_actor[3,Intercept]   -1.473     0.993   -3.109    0.013 1.000
## 9     r_actor[4,Intercept]   -1.478     0.992   -3.114    0.002 1.000
## 10    r_actor[5,Intercept]   -1.170     0.989   -2.808    0.305 1.000
## 11    r_actor[6,Intercept]   -0.220     0.988   -1.846    1.275 1.000
## 12    r_actor[7,Intercept]    1.320     1.014   -0.347    2.884 1.000
## 13    r_block[1,Intercept]   -0.173     0.224   -0.603    0.080 1.000
## 14    r_block[2,Intercept]    0.037     0.186   -0.238    0.359 1.000
## 15    r_block[3,Intercept]    0.053     0.186   -0.216    0.391 1.000
## 16    r_block[4,Intercept]    0.005     0.184   -0.294    0.310 1.000
## 17    r_block[5,Intercept]   -0.031     0.185   -0.355    0.253 1.000
## 18    r_block[6,Intercept]    0.111     0.201   -0.137    0.494 1.000
## 19                    lp__ -292.898     3.708 -299.459 -287.369 1.001
```

We might make the coefficient plot in Figure 12.4.a. like this:


```r
library(bayesplot)
color_scheme_set("orange")

stanplot(b12.5, pars = c("^b_", "^r_", "^sd_")) +
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

We might compare our models by their LOO-IC values.


```r
loo(b12.4, b12.5)
```

```
##                LOOIC    SE
## b12.4         531.63 19.48
## b12.5         532.59 19.70
## b12.4 - b12.5  -0.96  1.70
```

The two models yield nearly-equivalent information criteria values. Yet recall what McElreath wrote: “There is nothing to gain here by selecting either model. The comparison of the two models tells a richer story” (p. 367).

## 12.4. Multilevel posterior predictions

### 12.4.2 Posterior prediction for hew clusters. 

It'll take a bit of prep work to make Figure 12.5. First, let's glance at the model.


```r
print(b12.4)
```

```
##  Family: binomial 
##   Links: mu = logit 
## Formula: pulled_left ~ 1 + prosoc_left + prosoc_left:condition + (1 | actor) 
##    Data: d (Number of observations: 504) 
## Samples: 4 chains, each with iter = 5000; warmup = 1000; thin = 1; 
##          total post-warmup samples = 16000
##     ICs: LOO = NA; WAIC = NA; R2 = NA
##  
## Group-Level Effects: 
## ~actor (Number of levels: 7) 
##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## sd(Intercept)     2.27      0.97     1.13     4.77       3685 1.00
## 
## Population-Level Effects: 
##                       Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## Intercept                 0.42      0.98    -1.42     2.47       3268 1.00
## prosoc_left               0.82      0.26     0.31     1.34       9215 1.00
## prosoc_left:condition    -0.13      0.30    -0.73     0.46       8811 1.00
## 
## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
## is a crude measure of effective sample size, and Rhat is the potential 
## scale reduction factor on split chains (at convergence, Rhat = 1).
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
## 1 C00   0.587 0.336 0.833      1.00
## 2 C10   0.742 0.534 0.920      2.00
## 3 C01   0.587 0.336 0.833      3.00
## 4 C11   0.720 0.499 0.909      4.00
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

![](Ch._12_Multilevel_Models_files/figure-html/unnamed-chunk-41-1.png)<!-- -->

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

![](Ch._12_Multilevel_Models_files/figure-html/unnamed-chunk-43-1.png)<!-- -->

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

![](Ch._12_Multilevel_Models_files/figure-html/unnamed-chunk-44-1.png)<!-- -->

###1 2.4.3. Focus and multilevel prediction.

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

With brms, we don't actually need to make the logpop or society variables. We're ready to fit the multilevel Kline model with the data in hand.


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

Here is the data-processing work for my attempt to recreate Figure 12.6.


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
## $ Estimate   <dbl> 19.74889, 31.20844, 36.52856, 40.30622, 43.44589, 4...
## $ Est.Error  <dbl> 10.10435, 13.99823, 16.68815, 18.31889, 20.08987, 2...
## $ `1.5%ile`  <dbl> 5, 10, 12, 14, 15, 15, 16, 17, 17, 17, 18, 18, 19, ...
## $ `5.5%ile`  <dbl> 8, 15, 17, 19, 21, 22, 23, 24, 25, 25, 26, 26, 27, ...
## $ `16.5%ile` <dbl> 11, 20, 23, 26, 27, 29, 31, 32, 33, 34, 35, 36, 36,...
## $ `83.5%ile` <dbl> 27, 42, 48, 53, 57, 60, 63, 66, 69, 71, 72, 75, 77,...
## $ `94.5%ile` <dbl> 36.000, 53.000, 62.000, 68.000, 74.000, 79.000, 83....
## $ `98.5%ile` <dbl> 48.000, 71.000, 83.015, 92.000, 99.000, 105.000, 11...
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

![](Ch._12_Multilevel_Models_files/figure-html/unnamed-chunk-49-1.png)<!-- -->



Note. The analyses in this document were done with:

* R           3.4.4
* RStudio     1.1.442
* rmarkdown   1.9
* rstan       2.17.3
* rethinking  1.59
* brms        2.1.9
* tidyverse   1.2.1 
* ggthemes    3.4.0
* bayesplot   1.4.0

## References
Gelman, A., & Hill, J. (2007). *Data analysis using regression and multilevel/hierarchical models.* New York, NY, US: Cambridge University Press. 
McElreath, R. (2016). *Statistical rethinking: A Bayesian course with examples in R and Stan.* Chapman & Hall/CRC Press.
