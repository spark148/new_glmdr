# R package glmdr (Aug/31/2020)

## Exponential Family Generalized Linear Models Done Right

In the generalized linear model, the maximum likelihood estimate (MLE) does not exist in the conventional sense when solutions are "at infinity" in terms of canonical parameters or "on the boundary" in terms of mean value parameters. The glmdr deals with this case for the full discrete exponential families generalized linear models (binomial, poisson, multinomial, and product multinomial response). It provides valid hypothesis tests, confidence intervals and its corresponding summary. 
  
## Usage 

```r
library(devtools)
install_github(repo = "DEck13/complete_separation") #This would (?) be changed to the right path (e.g. cjgeyer/glmdr)
library(glmdr)
```

## Illustrative Example 

We provide our model-based solution to the completely degenerate logistic regression example. The data looks like below. 

<p align="center"><img src="./glmdr_example_dat.png" width=70%></div></p>

This data exhibits a complete separation and `glm` fails to provide useful information with error messages.
Specifically, MLE in the Barndorff-Nielsen completion is completely degenerate and this model does not have identifiable parameters.
Yet, we can still make a valid inference with one-sided confidence intervals for mean value paraemters.

```r
attach(quadratic)
glmdr_out <- glmdr(y ~ x + I(x^2),  family="binomial")
summary(glmdr_out)
glmdr_out_inf <- inference(glmdr_out, alpha =0.05)
glmdr_out_inf
```

Based on the confidence intervals, we can make a plot:

```r
plot(x, y, ylim = c(0,1), pch = 16, ylab = "", xlab = "")
points(x, glmdr_out_inf[, 1])
points(x, glmdr_out_inf[, 2])
segments(x, glmdr_out_inf[, 1], x, glmdr_out_inf[, 2])
```

<p align="center"><img src="./glmdr_example_1.png" width=71%></p>

This plot represents one-sided 95% confidence intervals for mean value parameters. Balck dots are the observed data, white dots are either upper or lower bound of the intervals and bars are the intervals. Horizontal axis shows the predictor value and vertical axis is the probability of observing response value = 1 at the corresponding predictor value.

## Further details

For more details, please see:

  Geyer, C.J. (2009)
  
  Likelihood inference in exponential families and directions of recession.

  Electronic Journal of Statistics, 3, 259-289.

  http://projecteuclid.org/euclid.ejs/1239716414.
  
<br>

  Eck, D.J. and Geyer, C.J. (submitted)
  
  Computationally efficient likelihood inference in exponential families when the maximum likelihood estimator does not exist.
  
  https://arxiv.org/abs/1803.11240.

## Citation

To cite this package:
```r
citation("glmdr")
```

