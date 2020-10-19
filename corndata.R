

library(glmdr)
library(tidyverse)

dat <- read.csv("~/research/expofam/complete_separation/complete_separation/manuscript/Supplementary/Corn_data/Combined_Final_Product.csv")
head(dat)
Xind <- 11:ncol(dat)

## remove non-singularities in the design matrix 
## the indices removed were discovered via a glm call
X <- as.matrix(dat[, Xind])
eigen(crossprod(X))
foo <- dat[, c(10,8,Xind[-c(16,20,23,24,25)])]

## fit model
m1 <- glm(Kernel.color ~ -1 + ., data = foo, family = "binomial")
summary(m1)


## Suyoung's version of glmdr which uses nloptr
m2 <- glmdr(Kernel.color ~., data = foo, family = "binomial")

## this is fast, great!
mus.CI <- inference(m2)
mus.CI

m2.lcm <- glmdr(Kernel.color ~., data = foo[m2$linearity, ], family = "binomial")
summary(m2.lcm)


m2.lcm <- glm(Kernel.color ~., data = foo[m2$linearity, ], family = "binomial")
preds <- as.numeric(predict(m2.lcm, type = "response"))
sort(preds)
sort(1 - preds)
