# Statistical Rethinking with brms, ggplot2, and the tidyverse

## This is a love letter

I love McElreath’s [*Statistical Rethinking*  text](http://xcelab.net/rm/statistical-rethinking/). It's the entry-level textbook for applied researchers I spent a couple years looking for. McElreath's [freely-available lectures](https://www.youtube.com/channel/UCNJK6_DZvcMqNSzQdEkzvzA/playlists) on the book are really great, too.

However, I've come to prefer using Bürkner’s [brms package](https://github.com/paul-buerkner/brms) when doing Bayeisn regression in R. [It's just spectacular.](http://andrewgelman.com/2017/01/10/r-packages-interfacing-stan-brms/) I also prefer plotting with Wickham's [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html), and recently converted to using [tidyverse](https://www.tidyverse.org)-style syntax (which you might learn about [here](http://r4ds.had.co.nz/transform.html) or [here](http://style.tidyverse.org)).

So, this project is an attempt to reexpress the code in McElreath’s textbook. His models are re-fit in brms, plots are reproduced or reimagined with ggplot2, and the general data wrangling code now predominantly follows the tidyverse style. 

This project is not meant to stand alone. It's a supplement to the textbook. Starting with chapter 4--except chapter 9, which doesn't have any models--, each chapter from *Statistical Rethinking* has its own document available in either an [R Notebook file](http://rmarkdown.rstudio.com/r_notebooks.html) or the corresponding HTML document. The flow matches closely to the book, but once in a while I add a little something extra. For example, the end of the chapter 5 files digress on the Bayesian *R* squared. I also devote some space at end of the chapter 6 files to highlight the `brms::marginal_effects()` function.

I also play around with the plots, quite a bit. The plots in the chapter 4 documents most closely mirror those in the text. I take more liberties in the other files. This is particularly aimed at newer ggplot2 users, to give them a sense of what's possible.

**Disclaimer**: brms is a rapidly-evolving package. To get a sense, see how frequently Bürkner has updated some of the most [recent versions](https://cran.r-project.org/src/contrib/Archive/brms/). I also have a lot to learn as a Bayesian and as an R user. So some of the code may appear dated or inelegant and I'm not yet sure how to reproduce some of the models/plots in the text. Which is all to say, *suggestions on how to improve my code are welcome*.

Happy modeling!
