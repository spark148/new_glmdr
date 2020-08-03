# complete_separation - updated Aug 3rd

This is the research work in the glmdr, which deals with the case when the canonical statistic(s) lie(s) on the boundary of its convext support.

## Description:

1. glmdr_main: main function of the research. We import/fit/process the data here.
2. my_inference: revised inference() function. Currently only support for the logistic regression case (including Bradley-Terry model).



## Order to run:
1. my_inference.R (first)
2. glmdr_main.R (second)

## Current problem / in progress:
1. - [x] Computational error in completly degenerate logsitic regression case when LCM space is equal to the whole parameter space. </br>
&nbsp;- currently investigating the auglag function

2. - [ ] Deviance

## Future task:
1. Adding Poisson case in the inference()
