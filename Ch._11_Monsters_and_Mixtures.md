---
title: "Ch. 11 Monsters and Mixtures"
author: "A Solomon Kurz"
date: "2018-03-25"
output:
  html_document:
    code_folding: show
    keep_md: TRUE
---

## 11.1. Ordered categorical outcomes

### 11.1.1. Example: Moral intuition.

Let's get the `Trolley` data from rethinking.


```r
library(rethinking)
data(Trolley)
d <- Trolley
```

Unload rethinking and load brms.


```r
rm(Trolley)
detach(package:rethinking, unload = T)
library(brms)
```

### 11.1.2. Describing an ordered distribution with intercepts.

Before we get to plotting, in this manuscript we'll use theme settings and a color palette from the [ggthemes package](https://cran.r-project.org/web/packages/ggthemes/index.html), which you might learn more about [here](https://cran.r-project.org/web/packages/ggthemes/vignettes/ggthemes.html).


```r
library(ggthemes)
```

We'll take our basic theme settings from the `theme_hc()` function. We'll use the `Green fields` color palette, which we can inspect with the `canva_pal()` function and a little help from `scales::show_col()`.


```r
scales::show_col(canva_pal("Green fields")(4))
```

![](Ch._11_Monsters_and_Mixtures_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
canva_pal("Green fields")(4)
```

```
## [1] "#919636" "#524a3a" "#fffae1" "#5a5f37"
```

```r
canva_pal("Green fields")(4)[3]
```

```
## [1] "#fffae1"
```

Our ggplot2 version of the simple histogram, Figure 11.1.a.


```r
library(tidyverse)

ggplot(data = d, aes(x = response, fill = ..x..)) +
  geom_histogram(binwidth = 1/4, size = 0) +
  scale_x_continuous(breaks = 1:7) +
  theme_hc() +
  scale_fill_gradient(low = canva_pal("Green fields")(4)[4],
                      high = canva_pal("Green fields")(4)[1]) +
  theme(axis.ticks.x = element_blank(),
        plot.background = element_rect(fill = "grey92"),
        legend.position = "none")
```

![](Ch._11_Monsters_and_Mixtures_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Our cumulative proportion plot, Figure 11.1.b.


```r
d %>%
  group_by(response) %>% 
  count() %>%
  mutate(pr_k = n/nrow(d)) %>% 
  ungroup() %>% 
  mutate(cum_pr_k = cumsum(pr_k)) %>% 
  
  ggplot(aes(x = response, y = cum_pr_k, 
             fill = response)) +
  geom_line(color = canva_pal("Green fields")(4)[2]) +
  geom_point(shape = 21, colour = "grey92", 
             size = 2.5, stroke = 1) +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(breaks = c(0, .5, 1)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(y = "cumulative proportion") +
  theme_hc() +
  scale_fill_gradient(low = canva_pal("Green fields")(4)[4],
                      high = canva_pal("Green fields")(4)[1]) +
  theme(axis.ticks.x = element_blank(),
        plot.background = element_rect(fill = "grey92"),
        legend.position = "none")
```

![](Ch._11_Monsters_and_Mixtures_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

In order to make the next plot, we'll need McElreath's `logit()` function, which is not to be confused with Gelman and Hill's (2007, p 91.) `invlogit()` function. Here it is, the logarithm of cumulative odds plot, Figure 11.1.c.


```r
# McElreath's convenience function
logit <- function(x) log(x/(1-x))

d %>%
  group_by(response) %>% 
  count() %>%
  mutate(pr_k = n/nrow(d)) %>% 
  ungroup() %>% 
  mutate(cum_pr_k = cumsum(pr_k)) %>% 
  filter(response < 7) %>% 
  
  # We can do the logit() conversion right in ggplot2
  ggplot(aes(x = response, y = logit(cum_pr_k), 
             fill = response)) +
  geom_line(color = canva_pal("Green fields")(4)[2]) +
  geom_point(shape = 21, colour = "grey92", 
             size = 2.5, stroke = 1) +
  scale_x_continuous(breaks = 1:7) +
  coord_cartesian(xlim = c(1, 7)) +
  labs(y = "log-cumulative-odds") +
  theme_hc() +
  scale_fill_gradient(low = canva_pal("Green fields")(4)[4],
                      high = canva_pal("Green fields")(4)[1]) +
  theme(axis.ticks.x = element_blank(),
        plot.background = element_rect(fill = "grey92"),
        legend.position = "none")
```

![](Ch._11_Monsters_and_Mixtures_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Here's Figure 11.2.


```r
d_plot <-
  d %>%
  group_by(response) %>% 
  count() %>%
  mutate(pr_k = n/nrow(d)) %>% 
  ungroup() %>% 
  mutate(cum_pr_k = cumsum(pr_k)) 

ggplot(data = d_plot,
       aes(x = response, y = cum_pr_k, 
             color = cum_pr_k, fill = cum_pr_k)) +
  geom_line(color = canva_pal("Green fields")(4)[1]) +
  geom_point(shape = 21, colour = "grey92", 
             size = 2.5, stroke = 1) +
  geom_linerange(aes(ymin = 0, ymax = cum_pr_k),
                 alpha = 1/2, color = canva_pal("Green fields")(4)[1]) +
  # There are probably more elegant ways to do this part.
  geom_linerange(data = . %>% 
                   mutate(discrete_probability =
                            ifelse(response == 1, cum_pr_k,
                                   cum_pr_k - pr_k)),
                 aes(x = response + .025,
                     ymin = ifelse(response == 1, 0, discrete_probability), 
                     ymax = cum_pr_k),
                 color = "black") +
  geom_text(data = tibble(text        = 1:7,
                          response = seq(from = 1.25, to = 7.25, by = 1),
                          cum_pr_k = d_plot$cum_pr_k - .065),
            aes(label = text),
            size = 4) +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(breaks = c(0, .5, 1)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(y = "cumulative proportion") +
  theme_hc() +
  scale_fill_gradient(low = canva_pal("Green fields")(4)[4],
                      high = canva_pal("Green fields")(4)[1]) +
  scale_color_gradient(low = canva_pal("Green fields")(4)[4],
                       high = canva_pal("Green fields")(4)[1]) +
  theme(axis.ticks.x = element_blank(),
        plot.background = element_rect(fill = "grey92"),
        legend.position = "none")
```

![](Ch._11_Monsters_and_Mixtures_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Whereas in `rethinking::map()` you indicate the likelihood by `<criterion> ~ dordlogit(phi , c(<the thresholds>)`, in `brms::brm()` you code `family = cumulative`. Here's the intercepts (i.e., thresholds) only model:


```r
# Here are our starting values, which we specify with the `inits` argument in brm()
Inits <- list(`Intercept[1]` = -2,
              `Intercept[2]` = -1,
              `Intercept[3]` = 0,
              `Intercept[4]` = 1,
              `Intercept[5]` = 2,
              `Intercept[6]` = 2.5)

InitsList <-list(Inits, Inits)

b11.1 <- 
  brm(data = d, family = cumulative,
      response ~ 1,
      prior = c(set_prior("normal(0, 10)", class = "Intercept")),
      iter = 2000, warmup = 1000, cores = 2, chains = 2,
      inits = InitsList)  # Here we place our start values into brm()
```

McElreath needed to include the `depth = 2` argument in the `rethinking::precis()` function to show the threshold parameters. With a `brm()` fit, we just use `print()` or `summary()` as usual.


```r
print(b11.1)
```

```
##  Family: cumulative 
##   Links: mu = logit; disc = identity 
## Formula: response ~ 1 
##    Data: d (Number of observations: 9930) 
## Samples: 2 chains, each with iter = 2000; warmup = 1000; thin = 1; 
##          total post-warmup samples = 2000
##     ICs: LOO = NA; WAIC = NA; R2 = NA
##  
## Population-Level Effects: 
##              Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## Intercept[1]    -1.92      0.03    -1.98    -1.86       1414 1.00
## Intercept[2]    -1.27      0.02    -1.32    -1.22       1780 1.00
## Intercept[3]    -0.72      0.02    -0.76    -0.68       2000 1.00
## Intercept[4]     0.25      0.02     0.21     0.29       2000 1.00
## Intercept[5]     0.89      0.02     0.85     0.94       2000 1.00
## Intercept[6]     1.77      0.03     1.72     1.83       2000 1.00
## 
## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
## is a crude measure of effective sample size, and Rhat is the potential 
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

The summaries look like those in the text, number of effective samples are high, and the Rhat values are great. The model looks good.

Here we can actually use Gelman and Hill's `invlogit()` function in place of McElreath's `logistic()` function.


```r
invlogit <- function(x){1/(1+exp(-x))}

b11.1 %>% 
  fixef() %>% 
  invlogit()
```

```
##               Estimate Est.Error   2.5%ile  97.5%ile
## Intercept[1] 0.1282109 0.5075275 0.1217571 0.1349123
## Intercept[2] 0.2197400 0.5061035 0.2115658 0.2281225
## Intercept[3] 0.3275875 0.5054163 0.3184470 0.3369418
## Intercept[4] 0.5616728 0.5050932 0.5521265 0.5714893
## Intercept[5] 0.7090201 0.5055663 0.6999016 0.7183252
## Intercept[6] 0.8545123 0.5070497 0.8476393 0.8613094
```

### 11.1.3. Adding predictor variables.

I'm not aware that brms has an equivalent to the `rethinking::dordlogit()` function. So here we'll make it by hand. The code comes from McElreath's [GitHub page](https://github.com/rmcelreath/rethinking/blob/a309712d904d1db7af1e08a76c521ab994006fd5/R/distributions.r).


```r
# First, we needed to specify the logistic() function, which is apart of the dordlogit() function
logistic <- function(x) {
    p <- 1 / (1 + exp(-x))
    p <- ifelse(x == Inf, 1, p)
    p
    }

# Now we get down to it
dordlogit <- 
  function(x, phi, a, log = FALSE) {
    a  <- c(as.numeric(a), Inf)
    p  <- logistic(a[x] - phi)
    na <- c(-Inf, a)
    np <- logistic(na[x] - phi)
    p  <- p - np
    if (log == TRUE) p <- log(p)
    p
    }
```

The `dordlogit()` function works like this.


```r
(pk <- dordlogit(1:7, 0, fixef(b11.1)[, 1]))
```

```
## [1] 0.12821091 0.09152904 0.10784752 0.23408537 0.14734727 0.14549219
## [7] 0.14548770
```

Note the slight difference in how we used `dordlogit()` with a `brm()` fit summarized by `fixef()` than the way McElreath did with a `map2stan()` fit summarized by `coef()`. McElreath just put `coef(m11.1)` into `dordlogit()`. We, however, more specifically placed `fixef(b11.1)[, 1]` into the function. With the `[, 1]` part, we specified that we were working with the posterior means (i.e., `Estimate`) and neglecting the other summaries (i.e., the posterior *SD*s and 95% intervals). If you forget to do this, chaos ensues.

Next, as McElreath further noted in the text, "these probabilities imply an average outcome of:"


```r
sum(pk*(1:7))
```

```
## [1] 4.199256
```
 
I found that a bit abstract. Here's the thing in a more elaborate tibble format.


```r
(
  explicit_example <-
  tibble(probability_of_a_response = pk) %>%
  mutate(the_response = 1:7) %>%
  mutate(their_product = probability_of_a_response*the_response)
)
```

```
## # A tibble: 7 x 3
##   probability_of_a_response the_response their_product
##                       <dbl>        <int>         <dbl>
## 1                    0.128             1         0.128
## 2                    0.0915            2         0.183
## 3                    0.108             3         0.324
## 4                    0.234             4         0.936
## 5                    0.147             5         0.737
## 6                    0.145             6         0.873
## 7                    0.145             7         1.02
```

```r
explicit_example %>%
  summarise(average_outcome_value = sum(their_product))
```

```
## # A tibble: 1 x 1
##   average_outcome_value
##                   <dbl>
## 1                  4.20
```

**Side note**

This made me wonder how this would compare if we were lazy and ignored the categorical nature of the `response`. Here we refit the model with the typical Gaussian likelihood.


```r
brm(data = d, family = gaussian,
    response ~ 1,
    # In this case, 4 (i.e., the middle response) seems to be the conservative place to put the mean
    prior = c(set_prior("normal(4, 10)", class = "Intercept"),
              set_prior("cauchy(0, 1)", class = "sigma")),
    iter = 2000, warmup = 1000, cores = 4, chains = 4) %>%
  print()
```

```
##  Family: gaussian 
##   Links: mu = identity; sigma = identity 
## Formula: response ~ 1 
##    Data: d (Number of observations: 9930) 
## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1; 
##          total post-warmup samples = 4000
##     ICs: LOO = NA; WAIC = NA; R2 = NA
##  
## Population-Level Effects: 
##           Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## Intercept     4.20      0.02     4.16     4.24       2843 1.00
## 
## Family Specific Parameters: 
##       Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## sigma     1.91      0.01     1.88     1.93       3621 1.00
## 
## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
## is a crude measure of effective sample size, and Rhat is the potential 
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

Happily, this yielded a mean estimate of 4.2, much like our `average_outcome_value`, above.

**End side note**

Now we'll try it by subtracting .5 from each.
 

```r
# The probabilities of a given response
(pk <- dordlogit(1:7, 0, fixef(b11.1)[, 1] - .5))
```

```
## [1] 0.08189522 0.06399785 0.08219883 0.20922807 0.15911432 0.18438366
## [7] 0.21918205
```

```r
# The average rating
sum(pk*(1:7))
```

```
## [1] 4.729548
```

So the rule is we *subtract the linear model from each interecept*. Let's fit our multivariable models.


```r
# Start values for b11.2
Inits <- list(`Intercept[1]` = -1.9,
              `Intercept[2]` = -1.2,
              `Intercept[3]` = -0.7,
              `Intercept[4]` = 0.2,
              `Intercept[5]` = 0.9,
              `Intercept[6]` = 1.8,
              action = 0,
              intention = 0,
              contact = 0)

b11.2 <- 
  brm(data = d, family = cumulative,
      response ~ 1 + action + intention + contact,
      prior = c(set_prior("normal(0, 10)", class = "Intercept"),
                set_prior("normal(0, 10)", class = "b")),
      iter = 2000, warmup = 1000, cores = 2, chains = 2,
      inits = list(Inits, Inits))

# Start values for b11.3
Inits <- list(`Intercept[1]` = -1.9,
              `Intercept[2]` = -1.2,
              `Intercept[3]` = -0.7,
              `Intercept[4]` = 0.2,
              `Intercept[5]` = 0.9,
              `Intercept[6]` = 1.8,
              action = 0,
              intention = 0,
              contact = 0,
              `action:intention` = 0,
              `contact:intention` = 0)

b11.3 <- 
  brm(data = d, family = cumulative,
      response ~ 1 + action + intention + contact +
        action:intention + contact:intention,
      prior = c(set_prior("normal(0, 10)", class = "Intercept"),
                set_prior("normal(0, 10)", class = "b")),
      iter = 2000, warmup = 1000, cores = 2, chains = 2,
      inits = list(Inits, Inits))
```

We don't have a `coeftab()` function in brms like for rethinking. But as we did for the chapter 6 supplement, we can reproduce it with help from the [broom package](https://cran.r-project.org/web/packages/broom/index.html) and a bit of data wrangling.


```r
library(broom)

tidy(b11.1) %>% mutate(model = "b11.1") %>% 
  bind_rows(tidy(b11.2) %>% mutate(model = "b11.2")) %>% 
  bind_rows(tidy(b11.3) %>% mutate(model = "b11.3")) %>% 
  select(model, term, estimate) %>% 
  filter(term != "lp__") %>% 
  complete(term = distinct(., term), model) %>% 
  mutate(estimate = round(estimate, digits = 2)) %>%
  spread(key = model, value = estimate) %>% 
  slice(c(6:11, 1, 4, 3, 2, 5))  # Here we indicate the order we'd like the rows in
```

```
## # A tibble: 11 x 4
##    term                  b11.1   b11.2  b11.3
##    <chr>                 <dbl>   <dbl>  <dbl>
##  1 b_Intercept[1]      - 1.92  - 2.84  -2.64 
##  2 b_Intercept[2]      - 1.27  - 2.16  -1.94 
##  3 b_Intercept[3]      - 0.720 - 1.57  -1.34 
##  4 b_Intercept[4]        0.250 - 0.550 -0.310
##  5 b_Intercept[5]        0.890   0.120  0.360
##  6 b_Intercept[6]        1.77    1.02   1.27 
##  7 b_action             NA     - 0.710 -0.470
##  8 b_intention          NA     - 0.720 -0.280
##  9 b_contact            NA     - 0.960 -0.330
## 10 b_action:intention   NA      NA     -0.450
## 11 b_intention:contact  NA      NA     -1.27
```

If you really wanted that last `nobs` row at the bottom, you could elaborate on this code: `b11.1$data %>% count()`. Also, if you want a proper `coeftab()` function for brms, McElreath's code lives [here](https://github.com/rmcelreath/rethinking/blob/a309712d904d1db7af1e08a76c521ab994006fd5/R/coeftab.r). Give it a whirl.

Anyway, here are the WAIC comparisons. *Caution: This took some time to compute.*


```r
waic(b11.1, b11.2, b11.3)
```

```
##                   WAIC    SE
## b11.1         37854.70 57.74
## b11.2         37090.22 76.34
## b11.3         36929.43 81.19
## b11.1 - b11.2   764.47 56.05
## b11.1 - b11.3   925.27 62.62
## b11.2 - b11.3   160.80 25.70
```

McElreath made Figure 11.3 by extracting the samples of his `m11.3`, saving them as `post`, and working some hairy base R `plot()` code. We'll take a different route and use `brms::fitted()`. This will take substantial data wrangling, but hopefully it'll be instructive. Let's first take a look at the initial `fitted()` output for the beginnings of Figure 11.3.a.


```r
nd <-
  tibble(action = 0,
         contact = 0, 
         intention = 0:1)

max_iter <- 100

fitted(b11.3, 
        newdata = nd, 
        subset = 1:max_iter,
        summary = F) %>% 
  as_tibble() %>% 
  glimpse()
```

```
## Observations: 100
## Variables: 14
## $ `1.1` <dbl> 0.06574897, 0.06837233, 0.06723808, 0.06291652, 0.067865...
## $ `2.1` <dbl> 0.08828919, 0.08753322, 0.08507545, 0.08444881, 0.090211...
## $ `1.2` <dbl> 0.05453787, 0.05834497, 0.05660048, 0.05667126, 0.059912...
## $ `2.2` <dbl> 0.07006558, 0.07189761, 0.06913342, 0.07281135, 0.076119...
## $ `1.3` <dbl> 0.07617039, 0.08473382, 0.08177152, 0.08064785, 0.077124...
## $ `2.3` <dbl> 0.09337845, 0.10010761, 0.09609623, 0.09866824, 0.093463...
## $ `1.4` <dbl> 0.2159296, 0.2171239, 0.2134291, 0.2202882, 0.2205542, 0...
## $ `2.4` <dbl> 0.2395399, 0.2355028, 0.2316748, 0.2433115, 0.2423256, 0...
## $ `1.5` <dbl> 0.1646769, 0.1682133, 0.1683372, 0.1682744, 0.1657195, 0...
## $ `2.5` <dbl> 0.1611999, 0.1642040, 0.1654445, 0.1637356, 0.1611101, 0...
## $ `1.6` <dbl> 0.1913886, 0.1893660, 0.1921527, 0.1884176, 0.1888334, 0...
## $ `2.6` <dbl> 0.1678871, 0.1684999, 0.1727502, 0.1643939, 0.1652104, 0...
## $ `1.7` <dbl> 0.2315476, 0.2138456, 0.2204710, 0.2227841, 0.2199897, 0...
## $ `2.7` <dbl> 0.1796399, 0.1722548, 0.1798254, 0.1726305, 0.1715592, 0...
```

Hopefully by now it’s clear why we needed the `nd` tibble, which we made use of in the `newdata = nd` argument. Because we set `summary = F`, we get draws from the posterior instead of summaries. With `max_iter`, we controlled how many of those posterior draws we wanted. McElreath used 100, which he indicated at the top of page 341, so we followed suit. It took me a minute to wrap my head around the meaning of the 14 vectors, which were named by `brms::fitted()` default. Notice how each column is named by two numerals, separated by a period. That first numeral indicates which if the two `intention` values the draw is based on (i.e., 1 stands for intention = 0, 2, stands for intention = 1). The numbers on the right of the decimals are the seven response options for `response`. For each posterior draw, you get one of those for each value of `intention`. Finally, it might not be immediately apparent, but the values are in the probability scale, just like `pk` on page 338.

Now we know what we have in hand, it’s just a matter of careful data wrangling to get those probabilities into a more useful format to insert into ggplot2. I’ve extensively annotated the code, below. If you lose track of happens in a given step, just run the code up till that point. Go step by step.


```r
nd <-
  tibble(action = 0,
         contact = 0, 
         intention = 0:1)

max_iter <- 100

fitted(b11.3, 
        newdata = nd, 
        subset = 1:max_iter,
        summary = F) %>% 
  as_tibble() %>%
  # We convert the data to the long format
  gather() %>%
  # We need an variable to index which posterior iteration we're working with
  mutate(iter = rep(1:max_iter, times = 14)) %>%
  # This step isn’t technically necessary, but I prefer my iter index at the far left.
  select(iter, everything()) %>% 
  # Here we extract the `intention` and `response` information out of the `key` vector and spread it into two vectors.
  separate(key, into = c("intention", "rating")) %>% 
  # That step produced two character vectors. They’ll be more useful as numbers
  mutate(intention = intention %>% as.double(),
         rating =  rating %>% as.double()) %>%
  # Here we convert `intention` into its proper 0:1 metric
  mutate(intention = intention -1) %>%
  # This isn't necessary, but it helps me understand exactly what metric the values are currently in
  rename(pk = value) %>% 
  # This step is based on McElreath's R code 11.10 on page 338
  mutate(`pk:rating` = pk*rating) %>% 
  # I’m not sure how to succinctly explain this. You’re just going to have to trust me.
  group_by(iter, intention) %>% 
  # This is very important for the next step.
  arrange(iter, intention, rating) %>% 
  # Here we take our `pk` values and make culmulative sums. Why? Take a long hard look at Figure 11.2. 
  mutate(probability = cumsum(pk)) %>% 
  # `rating == 7` is unnecessary. These `probability` values are by definition 1.
  filter(rating < 7) %>% 
  
  ggplot(aes(x = intention, 
             y = probability, 
             color = probability)) +
  geom_line(aes(group = interaction(iter, rating)),
            alpha = 1/10) +
  # Note how we made a new data object for geom_text()
  geom_text(data = tibble(text        = 1:7,
                          intention   = seq(from = .9, to = .1, length.out = 7),
                          probability = c(.05, .12, .20, .35, .53, .71, .87)),
            aes(label = text),
            size = 3) +
  scale_x_continuous(breaks = 0:1) +
  scale_y_continuous(breaks = c(0, .5, 1)) +
  coord_cartesian(ylim = 0:1) +
  labs(subtitle = "action = 0,\ncontact = 0",
       x = "intention") +
  theme_hc() +
  scale_color_gradient(low = canva_pal("Green fields")(4)[4],
                       high = canva_pal("Green fields")(4)[1]) +
  theme(plot.background = element_rect(fill = "grey92"),
        legend.position = "none")
```

![](Ch._11_Monsters_and_Mixtures_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

Boom!

Okay, that pile of code is a bit of a mess and you’re not going to want to repeatedly cut and paste all that. Let’s condense it into a homemade function, `make_Figure_11.3_data()`. 


```r
make_Figure_11.3_data <- function(action, contact, max_iter){
  
  nd <-
    tibble(action = action,
           contact = contact, 
           intention = 0:1)
  
  max_iter <- max_iter
  
  fitted(b11.3, 
         newdata = nd, 
         subset = 1:max_iter,
         summary = F) %>% 
    as_tibble() %>%
    gather() %>%
    mutate(iter = rep(1:max_iter, times = 14)) %>%
    select(iter, everything()) %>% 
    separate(key, into = c("intention", "rating")) %>% 
    mutate(intention = intention %>% as.double(),
           rating =  rating %>% as.double()) %>%
    mutate(intention = intention -1) %>%
    rename(pk = value) %>% 
    mutate(`pk:rating` = pk*rating) %>% 
    group_by(iter, intention) %>% 
    arrange(iter, intention, rating) %>% 
    mutate(probability = cumsum(pk)) %>% 
    filter(rating < 7) 
}
```

Now we'll use our sweet homemade function to make our plots.


```r
# Figure 11.3.a
make_Figure_11.3_data(action = 0, 
                      contact = 0, 
                      max_iter = 100) %>% 
  
  ggplot(aes(x = intention, 
             y = probability,
             color = probability)) +
  geom_line(aes(group = interaction(iter, rating)),
            alpha = 1/10) +
  geom_text(data = tibble(text        = 1:7,
                          intention   = seq(from = .9, to = .1, length.out = 7),
                          probability = c(.05, .12, .20, .35, .53, .71, .87)),
            aes(label = text),
            size = 3) +
  scale_x_continuous(breaks = 0:1) +
  scale_y_continuous(breaks = c(0, .5, 1)) +
  coord_cartesian(ylim = 0:1) +
  labs(subtitle = "action = 0,\ncontact = 0",
       x = "intention") +
  theme_hc() +
  scale_color_gradient(low = canva_pal("Green fields")(4)[4],
                       high = canva_pal("Green fields")(4)[1]) +
  theme(plot.background = element_rect(fill = "grey92"),
        legend.position = "none")
```

![](Ch._11_Monsters_and_Mixtures_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

```r
# Figure 11.3.b
make_Figure_11.3_data(action = 1, 
                      contact = 0, 
                      max_iter = 100) %>% 
  
  ggplot(aes(x = intention, 
             y = probability,
             color = probability)) +
  geom_line(aes(group = interaction(iter, rating)),
            alpha = 1/10) +
  geom_text(data = tibble(text        = 1:7,
                          intention   = seq(from = .9, to = .1, length.out = 7),
                          probability = c(.12, .24, .35, .50, .68, .80, .92)),
            aes(label = text),
            size = 3) +
  scale_x_continuous(breaks = 0:1) +
  scale_y_continuous(breaks = c(0, .5, 1)) +
  coord_cartesian(ylim = 0:1) +
  labs(subtitle = "action = 1,\ncontact = 0",
       x = "intention") +
  theme_hc() +
  scale_color_gradient(low = canva_pal("Green fields")(4)[4],
                       high = canva_pal("Green fields")(4)[1]) +
  theme(plot.background = element_rect(fill = "grey92"),
        legend.position = "none")
```

![](Ch._11_Monsters_and_Mixtures_files/figure-html/unnamed-chunk-24-2.png)<!-- -->

```r
# Figure 11.3.c
make_Figure_11.3_data(action = 0, 
                      contact = 1, 
                      max_iter = 100) %>% 
  
  ggplot(aes(x = intention, 
             y = probability,
             color = probability)) +
  geom_line(aes(group = interaction(iter, rating)),
            alpha = 1/10) +
  geom_text(data = tibble(text        = 1:7,
                          intention   = seq(from = .9, to = .1, length.out = 7),
                          probability = c(.15, .34, .44, .56, .695, .8, .92)),
            aes(label = text),
            size = 3) +
  scale_x_continuous(breaks = 0:1) +
  scale_y_continuous(breaks = c(0, .5, 1)) +
  coord_cartesian(ylim = 0:1) +
  labs(subtitle = "action = 0,\ncontact = 1",
       x = "intention") +
  theme_hc() +
  scale_color_gradient(low = canva_pal("Green fields")(4)[4],
                       high = canva_pal("Green fields")(4)[1]) +
  theme(plot.background = element_rect(fill = "grey92"),
        legend.position = "none")
```

![](Ch._11_Monsters_and_Mixtures_files/figure-html/unnamed-chunk-24-3.png)<!-- -->

If you really wanted to get crazy, you could save each of the three plots as objects and then feed them into the [`multiplot()` function](http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/). I'll leave that up to you. 

**Bonus**

I have a lot of respect for McElreath. But man, Figure 11.3 is the worst. I'm in clinical psychology and there's no way a working therapist is going to look at a figure like that and have any sense of what's going on. Nobody’s got time for that. We’ve have clients to serve! Happily, we can go further. Look back at McElreath’s R code 11.10 on page 338. See how he multiplied the elements of `pk` by their respective `response` values and then just summed them up to get an average outcome value? With just a little amendment to our custom `make_Figure_11.3_data()` function, we can wrangle our `fitted()` output to express average `response` values for each of our conditions of interest. Here’s the adjusted function:


```r
make_data_for_an_alternative_fiture <- function(action, contact, max_iter){
  
  nd <-
    tibble(action = action,
           contact = contact, 
           intention = 0:1)
  
  max_iter <- max_iter
  
  fitted(b11.3, 
         newdata = nd, 
         subset = 1:max_iter,
         summary = F) %>% 
    as_tibble() %>%
    gather() %>%
    mutate(iter = rep(1:max_iter, times = 14)) %>%
    select(iter, everything()) %>% 
    separate(key, into = c("intention", "rating")) %>% 
    mutate(intention = intention %>% as.double(),
           rating =  rating %>% as.double()) %>%
    mutate(intention = intention -1) %>%
    rename(pk = value) %>% 
    mutate(`pk:rating` = pk*rating) %>% 
    group_by(iter, intention) %>% 
    
    # Everything above this point is identical to the previous custom function.
    # All we do is replace the last few lines with this one line of code. 
    summarise(mean_rating = sum(`pk:rating`))
}
```

Our handy homemade but monstrously-named `make_data_for_an_alternative_fiture()` function works very much like its predecessor. You’ll see.


```r
# Alternative to Figure 11.3.a
make_data_for_an_alternative_fiture(action = 0, 
                                    contact = 0, 
                                    max_iter = 100) %>% 
  
  ggplot(aes(x = intention, y = mean_rating, group = iter)) +
  geom_line(alpha = 1/10, color = canva_pal("Green fields")(4)[1]) +
  scale_x_continuous(breaks = 0:1) +
  scale_y_continuous(breaks = 1:7) +
  coord_cartesian(ylim = 1:7) +
  labs(subtitle = "action = 0,\ncontact = 0",
       x = "intention",
       y = "response") +
  theme_hc() +
  theme(plot.background = element_rect(fill = "grey92"),
        legend.position = "none")
```

![](Ch._11_Monsters_and_Mixtures_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

```r
# Alternative to Figure 11.3.b
make_data_for_an_alternative_fiture(action = 1, 
                                    contact = 0, 
                                    max_iter = 100) %>% 
  
 ggplot(aes(x = intention, y = mean_rating, group = iter)) +
  geom_line(alpha = 1/10, color = canva_pal("Green fields")(4)[1]) +
  scale_x_continuous(breaks = 0:1) +
  scale_y_continuous(breaks = 1:7) +
  coord_cartesian(ylim = 1:7) +
  labs(subtitle = "action = 1,\ncontact = 0",
       x = "intention",
       y = "response") +
  theme_hc() +
  theme(plot.background = element_rect(fill = "grey92"),
        legend.position = "none")
```

![](Ch._11_Monsters_and_Mixtures_files/figure-html/unnamed-chunk-26-2.png)<!-- -->

```r
# Alternative to Figure 11.3.c
make_data_for_an_alternative_fiture(action = 0, 
                                    contact = 1, 
                                    max_iter = 100) %>% 
  
  ggplot(aes(x = intention, y = mean_rating, group = iter)) +
  geom_line(alpha = 1/10, color = canva_pal("Green fields")(4)[1]) +
  scale_x_continuous(breaks = 0:1) +
  scale_y_continuous(breaks = 1:7) +
  coord_cartesian(ylim = 1:7) +
  labs(subtitle = "action = 0,\ncontact = 1",
       x = "intention",
       y = "response") +
  theme_hc() +
  theme(plot.background = element_rect(fill = "grey92"),
        legend.position = "none")
```

![](Ch._11_Monsters_and_Mixtures_files/figure-html/unnamed-chunk-26-3.png)<!-- -->

Finally; now those are plots I can sell in a clinical psychology journal!

**End Bonus**

## 11.2. Zero-inflated outcomes

### 11.2.1. Example: Zero-inflated Poisson.

Here we simulate our drunk monk data.


```r
# define parameters
prob_drink <- 0.2  # 20% of days
rate_work  <- 1    # average 1 manuscript per day

# sample one year of production
N <- 365

# simulate days monks drink
set.seed(0.2)
drink <- rbinom(N, 1, prob_drink)

# simulate manuscripts completed
y <- (1 - drink)*rpois(N, rate_work)
```

And here we'll put those data in a tidy tibble before plotting.


```r
d <-
  tibble(Y = y) %>%
  arrange(Y) %>% 
  mutate(zeros = c(rep("zeros_drink", times = sum(drink)),
                   rep("zeros_work",  times = sum(y == 0 & drink == 0)),
                   rep("nope",        times = N - sum(y == 0))
                   )) 
  
  ggplot(data = d, aes(x = Y)) +
  geom_histogram(aes(fill = zeros),
                 binwidth = 1, color = "grey92") +
  scale_fill_manual(values = c(canva_pal("Green fields")(4)[1], 
                               canva_pal("Green fields")(4)[2], 
                               canva_pal("Green fields")(4)[1])) +
  xlab("Manuscripts completed") +
  theme_hc() +
  theme(plot.background = element_rect(fill = "grey92"),
        legend.position = "none")
```

![](Ch._11_Monsters_and_Mixtures_files/figure-html/unnamed-chunk-28-1.png)<!-- -->

The intercept [and zi] only zero-inflated Poisson model:


```r
b11.4 <- 
  brm(data = d, family = zero_inflated_poisson(),
      Y ~ 1,
      prior = c(set_prior("normal(0, 10)", class = "Intercept"),
                # This is the brms default. See below.
                set_prior("beta(1, 1)", class = "zi")),
      cores = 4)

print(b11.4)
```

```
##  Family: zero_inflated_poisson 
##   Links: mu = log; zi = identity 
## Formula: Y ~ 1 
##    Data: d (Number of observations: 365) 
## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1; 
##          total post-warmup samples = 4000
##     ICs: LOO = NA; WAIC = NA; R2 = NA
##  
## Population-Level Effects: 
##           Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## Intercept     0.05      0.08    -0.12     0.21        874 1.00
## 
## Family Specific Parameters: 
##    Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## zi     0.15      0.06     0.04     0.26        950 1.00
## 
## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
## is a crude measure of effective sample size, and Rhat is the potential 
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

The zero-inflated Poisson is [parameterized in brms](https://cran.r-project.org/web/packages/brms/vignettes/brms_families.html) a little differently than it is in rethinking. The different parameterization did not influence the estimate for the Intercept, $\lambda$. In both here and in the text, $\lambda$  was about 0.06. However, it did influence the summary of `zi`. Note how McElreaths `logistic(-1.39)` yielded 0.1994078. Seems rather close to our `zi` estimate of 0.15. First off, because he didn’t set his seed in the text before simulating (i.e., `set.seed(<insert some number>)`), we couldn’t exactly reproduce his simulated drunk monk data. So our results will vary a little due to that alone. But after accounting for simulation variance, hopefully it’s clear that `zi` in brms is already in the probability metric. There's no need to convert it.

Anyway, here's that exponentiated $\lambda$.


```r
fixef(b11.4)[1, ] %>%
  exp()
```

```
##  Estimate Est.Error   2.5%ile  97.5%ile 
##  1.049051  1.087757  0.890947  1.236907
```

## 11.3. Over-dispersed outcomes

### 11.3.1. Beta-binomial.


```r
pbar <- 0.5
theta <- 5

ggplot(data = tibble(x = seq(from = 0, to = 1, by = .01))) +
  geom_ribbon(aes(x = x, 
                  ymin = 0, 
                  ymax = rethinking::dbeta2(x, pbar, theta)),
              fill = canva_pal("Green fields")(4)[1]) +
  scale_x_continuous(breaks = c(0, .5, 1)) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = expression(paste("The ", beta, " distribution")),
       x = "probability space",
       y = "density") +
  theme_hc() +
  theme(plot.background = element_rect(fill = "grey92"))
```

![](Ch._11_Monsters_and_Mixtures_files/figure-html/unnamed-chunk-31-1.png)<!-- -->

The beta-binomial distribution is [not implimented in brms at this time](https://github.com/paul-buerkner/brms/issues/144). However, one can do something similar with a multilevel binomial model (foreshadow chapter 12!). Here are the `UCBadmit` data.


```r
library(rethinking)
data(UCBadmit)
d <- UCBadmit
```

Unload rethinking and load brms.


```r
rm(UCBadmit)
detach(package:rethinking, unload = T)
library(brms)
```

Now we fit our off-label model, `b11.5ish`.


```r
b11.5ish <-
  brm(data = d, family = binomial(),
      admit | trials(applications) ~ 1 + (1 | dept),
      prior = c(set_prior("normal(0, 10)", class = "Intercept"),
                set_prior("cauchy(0, 1)", class = "sd")),
      iter = 4000, warmup = 1000, cores = 2, chains = 2,
      control = list(adapt_delta = 0.95))
```

Here's our alternative model.


```r
print(b11.5ish)
```

```
##  Family: binomial 
##   Links: mu = logit 
## Formula: admit | trials(applications) ~ 1 + (1 | dept) 
##    Data: d (Number of observations: 12) 
## Samples: 2 chains, each with iter = 4000; warmup = 1000; thin = 1; 
##          total post-warmup samples = 6000
##     ICs: LOO = NA; WAIC = NA; R2 = NA
##  
## Group-Level Effects: 
## ~dept (Number of levels: 6) 
##               Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## sd(Intercept)     1.34      0.49     0.75     2.68        915 1.00
## 
## Population-Level Effects: 
##           Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## Intercept    -0.63      0.58    -1.75     0.54        786 1.00
## 
## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
## is a crude measure of effective sample size, and Rhat is the potential 
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

We'll use the old `invlogit()` function to convert the model intercept, which is on the log-odds scale, into a probability.


```r
b11.5ish %>% fixef() %>% 
  invlogit()
```

```
##            Estimate Est.Error   2.5%ile  97.5%ile
## Intercept 0.3470902 0.6408029 0.1479872 0.6310872
```

Given the output of `b11.5ish`, I'm not really sure how to create our analogue of Figure 11.5.a. If we stay in the logit scale, I suppose you could do:


```r
post <- posterior_samples(b11.5ish)

# post %>%
#   head()

tibble(x = seq(from = -6, to = 6, by = .1)) %>%
  
  ggplot(aes(x = x)) + 
  mapply(function(mean, sd) {
    stat_function(fun = dnorm,
                  args = list(mean = mean, sd = sd),
                  alpha = 1/4, size = 1/4,
                  color = canva_pal("Green fields")(4)[1]
                  )
  },
  # Enter means and standard deviations here
  mean = post[1:100, 1],
  sd   = post[1:100, 2]) +
  geom_line(aes(y = dnorm(x,
                          mean = fixef(b11.5ish)[1],
                          sd = VarCorr(b11.5ish)$dept$sd[1])),
            color = canva_pal("Green fields")(4)[4], size = 1.1) +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = -5:5) +
  theme_hc() +
  theme(plot.background = element_rect(fill = "grey92"),
        legend.position = "none")
```

![](Ch._11_Monsters_and_Mixtures_files/figure-html/unnamed-chunk-37-1.png)<!-- -->

But that's probably not very satisfying. If you can figure out a slick way to convert the lines to the probability scale, share your code!

We can, however, do our version of Figure 11.5.b.


```r
# The prediction intervals
predict(b11.5ish) %>%
  as_tibble() %>% 
  rename(LL = `2.5%ile`,
         UL = `97.5%ile`) %>%
  select(LL:UL) %>% 
  # The fitted intervals
  bind_cols(
    fitted(b11.5ish) %>%
  as_tibble()
  ) %>% 
  # The original data used to fit the model
  bind_cols(b11.5ish$data) %>% 
  mutate(case = 1:12) %>% 
  
  ggplot(aes(x = case)) +
  geom_linerange(aes(ymin = LL/applications, 
                     ymax = UL/applications),
                 color = canva_pal("Green fields")(4)[1], 
                 size = 2.25, alpha = 1/3) +
  geom_pointrange(aes(ymin = `2.5%ile`/applications, 
                      ymax = `97.5%ile`/applications, 
                      y = Estimate/applications),
                  color = canva_pal("Green fields")(4)[4],
                  size = 1/3, shape = 1) +
  geom_point(aes(y = admit/applications),
             color = canva_pal("Green fields")(4)[2]) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(breaks = c(0, .5, 1)) +
  coord_cartesian(ylim = 0:1) +
  labs(subtitle = "Posterior validation check",
       y = "Admittance probability") +
  theme_hc() +
  theme(plot.background = element_rect(fill = "grey92"),
        axis.ticks.x = element_blank(),
        legend.position = "none")
```

![](Ch._11_Monsters_and_Mixtures_files/figure-html/unnamed-chunk-38-1.png)<!-- -->

Our multilevel model did way better than the actual beta-binomial model in the text. See the next two chapters to understand why.

### 11.3.2. Negative-binomial or gamma-Poisson.


```r
mu <- 3
theta <- 1

ggplot(data = tibble(x = seq(from = 0, to = 12, by = .01)),
       aes(x = x)) +
  geom_ribbon(aes(ymin = 0, 
                  ymax = rethinking::dgamma2(x, mu, theta)),
              color = "transparent", 
              fill = canva_pal("Green fields")(4)[4]) +
  geom_vline(xintercept = mu, linetype = 3,
             color = canva_pal("Green fields")(4)[3]) +
  scale_x_continuous(NULL, breaks = c(0, mu, 10)) +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = 0:10) +
  ggtitle(expression(paste("Our sweet ", gamma))) +
  theme_hc() +
  theme(plot.background = element_rect(fill = "grey92"))
```

![](Ch._11_Monsters_and_Mixtures_files/figure-html/unnamed-chunk-39-1.png)<!-- -->

**Bonus**

McElreath didn't give an example of negative-binomial regression in the text. Here's one with the `UCBadmit` data.


```r
brm(data = d, family = negbinomial,
      admit ~ 1 + applicant.gender,
      prior = c(set_prior("normal(0, 10)", class = "Intercept"),
                set_prior("normal(0, 1)", class = "b"),
                set_prior("gamma(0.01, 0.01)", class = "shape")),  # this is the brms default
      iter = 4000, warmup = 1000, cores = 2, chains = 2,
      control = list(adapt_delta = 0.8)) %>% 
  
  print()
```

```
##  Family: negbinomial 
##   Links: mu = log; shape = identity 
## Formula: admit ~ 1 + applicant.gender 
##    Data: d (Number of observations: 12) 
## Samples: 2 chains, each with iter = 4000; warmup = 1000; thin = 1; 
##          total post-warmup samples = 6000
##     ICs: LOO = NA; WAIC = NA; R2 = NA
##  
## Population-Level Effects: 
##                      Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## Intercept                4.71      0.43     3.98     5.65       3088 1.00
## applicant.gendermale     0.57      0.51    -0.50     1.55       4081 1.00
## 
## Family Specific Parameters: 
##       Estimate Est.Error l-95% CI u-95% CI Eff.Sample Rhat
## shape     1.23      0.50     0.48     2.38       3435 1.00
## 
## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
## is a crude measure of effective sample size, and Rhat is the potential 
## scale reduction factor on split chains (at convergence, Rhat = 1).
```

Since the negative-binomial model uses the log link, you need to exponentiate to get the estimates back into the count metric. E.g.,


```r
exp(4.69)
```

```
## [1] 108.8532
```

Note. The analyses in this document were done with:

* R           3.4.4
* RStudio     1.1.442
* rmarkdown   1.9
* rethinking  1.59
* brms        2.1.9
* rstan       2.17.3
* ggthemes    3.4.0
* tidyverse   1.2.1
* broom       0.4.3

## Reference
McElreath, R. (2016). *Statistical rethinking: A Bayesian course with examples in R and Stan.* Chapman & Hall/CRC Press.


