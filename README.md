# R package glmdr - updated on Sep. 3rd

## Exponential Family Generalized Linear Models Done Right

In an exponential family generalized linear model (binomial, poisson, multinomial, and product multinomial response), the maximum likelihood estimate (MLE) does not exist in the conventional sense when solutions are "at infinity" (in terms of canonical parameters) or "on the boundary" (in terms of mean value parameters).
Functions for doing valid hypothesis tests and confidence intervals even in "solutions at infinity" cases are included.
    
## Usage 

```r
library(devtools)
install_github(repo = "DEck13/complete_separation")
library(nloptr)
```

## Illustrative Example 

We show that our model-based solution to the complete separation problem works, and that it provides narrower confidence intervals than competing methods. The data is constructed below. This data exhibits complete separation and \code{glm} provides a useless error message.
[Add more description].

```r
attach(quadratic)
glmdr_out <- glmdr(y ~ x + I(x^2),  family="binomial")
summary(glmdr_out)
glmdr_out_inf <- inference(glmdr_out)
glmdr_out_inf
```


[Add description]
```r
plot(x, y, ylim = c(0,1), pch = 16, ylab = "", xlab = "")
points(x, glmdr_out_inf[, 1])
points(x, glmdr_out_inf[, 2])
segments(x, glmdr_out_inf[, 1], x, glmdr_out_inf[, 2])
```

![Plot of confidence interval from glmdr](glmdr_example_1.png)



To cite this package:
```r
citation("glmdr")
```


## Further details

For more details, see:

  Eck, D.J. and Charles J. Geyer (2020+)
  Computationally efficient likelihood inference in exponential families when the maximum likelihood estimator does not exist.
