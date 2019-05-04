Important LaTeX bits
================
A Solomon Kurz
2019-05-04

What's the deal?
================

Offering the project in both HTML and PDF versions presents difficulties with some of the [LaTeX](https://www.latex-project.org) code. The issue stems from underscores. In some of the mathematical formulas in the project, one or more of the variables include underscores within the name. For example, the equation in section 5.1.1 contains two predictors, `Marriage_s` and `MedianAgeMarriage_s`. Both names end in the suffix `_s`. This doesn’t cause any problems when rendering to HTML. For example, the pertinent line of LaTeX is

\\mu\_i & = \\alpha + \\beta\_1 \\text{Marriage\_s}\_i + \\beta\_2 \\text{MedianAgeMarriage\_s}\_i

It renders beautifully. However, those `_1` suffixes produce errors when attempting to render to PDF. In that situation, the solution is to insert a backslash before the two underscores. It looks like this:

\\mu\_i & = \\alpha + \\beta\_1 \\text{Marriage\_s}\_i + \\beta\_2 \\text{MedianAgeMarriage\\\_s}\_i

When rendering to PDF, it works like a dream. However, when rendering to HTML, those extra backslashes get rendered as if there were part of the variable names. It’s a disaster. So the current fix is to document every place in the project including LaTeX syntax involving variable names including underscores. Happily, this only effects the `.Rmd` files for chapters 5, 7, 10, 12, and 13. Below I document each relevant section from each document with its line number.

How do I use this document?
===========================

The GitHub rendered version of this document (i.e., the `.md` file) is useful for understanding the problem. However, the rendering complications limit its utility for seeing the offending LaTeX code. For that, you’re probably best off checking out the `.Rmd` file.

Chapter 05
----------

line 214

line 1591

*μ*<sub>*i*</sub> = *α* + *β*<sub>clade\_nwm</sub>clade\_nwm<sub>*i*</sub> + *β*<sub>clade\_owm</sub>clade\_owm<sub>*i*</sub> + *β*<sub>clade\_s</sub>clade\_s<sub>*i*</sub> + *β*<sub>perc.fat</sub>perc.fat<sub>*i*</sub>

Chapter 07
----------

line 218

line 262

line 409

line 417

line 424

$$
\\mu\_i = \\alpha + \\beta\_1 \\text{rugged}\_i + \\underbrace{(\\beta\_2 + \\beta\_3 \\text{rugged}\_i)}\_G \\times \\text{cont\\\_africa}\_i
$$

Chapter 10
----------

line 51

line 265

line 864

Chapter 12
----------

line 542

line 649

line 1159

line 1259

line 1308

Chapter 13
----------

line 425

line 527

line 778

Session info
------------

``` r
sessionInfo()
```

    ## R version 3.5.1 (2018-07-02)
    ## Platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## Running under: macOS High Sierra 10.13.6
    ## 
    ## Matrix products: default
    ## BLAS: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] compiler_3.5.1  backports_1.1.4 magrittr_1.5    rprojroot_1.3-2
    ##  [5] tools_3.5.1     htmltools_0.3.6 yaml_2.1.19     Rcpp_1.0.1     
    ##  [9] stringi_1.4.3   rmarkdown_1.10  knitr_1.20      stringr_1.4.0  
    ## [13] digest_0.6.18   evaluate_0.10.1
