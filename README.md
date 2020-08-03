# complete_separation - updated Aug 3rd

This is the research work in the glmdr, which deals with the case when the canonical statistic(s) lie(s) on the boundary of its convext support.

## Description:

1. glmdr_main.R: main function of the research. We import/fit/process the data here.
2. my_inference.R: revised inference() function. Currently only support for the logistic regression case (including Bradley-Terry model).
3. inference_diff.R: to better compare the inference function between original and current one. In the history tab, we can see where the changes happened in the unifed/split view.

&nbsp;__\* glmdr is the same (no change).__


## Order to run:
1. my_inference.R (first)
2. glmdr_main.R (second)

## Current problem / in progress:
1. - [x] Computational error in completly degenerate logsitic regression case when LCM space is equal to the whole parameter space. </br>
&nbsp;- currently investigating the auglag function

2. - [ ] Deviance

## Future task:
1. Adding Poisson case in the inference()
