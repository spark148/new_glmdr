library(glmdr)
library(tidyverse)
library(readxl)
################PLEASE REVIEW BELOW####################
dat <- read_xlsx("./Combined_Final_Product.xlsx") ###Please set the correct path or correct function (e.g. read.csv)
################PLEASE REVIEW ABOVE####################
names(dat)[c(10,8)] <- c("Kernel.color","Pop.structure")
Xind <- 11:ncol(dat)
X <- as.matrix(dat[, Xind])
tmp <- cbind(dat[,c(10,8)],X)
#### Finding problem matic columns based on glm
tmp_mod <- glm(Kernel.color ~ ., data = tmp, family = "binomial")
which(is.na(coef(tmp_mod))) #finding problematic column
#### Check the (exact) collinearity in the X matrix.
sum(X[, "S6_82186654"] != X[, "S6_82186661"]) # 14th and 15th column are the same.
sum(X[, "S6_82218018"] != X[, "S6_82218044"]) # 18th and 19th column are the same.
sum(X[, "S6_82243856"] != X[, "S6_82243861"]) # 21, 22, 23 and 24th column are the same
sum(X[, "S6_82243861"] != X[, "S6_82243874"]) # 21, 22, 23 and 24th column are the same
sum(X[, "S6_82243874"] != X[, "S6_82243883"]) # 21, 22, 23 and 24th column are the same
####Dropping problematic columns and cleaned the model matrix.
foo <- dat[, c(10,8,Xind[-c(15,19,22,23,24)])] #10 = Kenerl color, 8 = Pop structure
foo$Pop.structure <- factor(foo$Pop.structure) # chr -> factor (to make sure everything works as expected.)
#### 1) fit GLM model using all observations
m1_glm <- glm(Kernel.color ~ ., data = foo, family = "binomial")
summary(m1_glm)
#### 2) send glm to the infinity | Complete separation presents
m1_inf <- glm(Kernel.color ~., data = foo, family = "binomial"(link="logit"), control = list(maxit=10000,epsilon=1e-50))
m1_inf
#### 3) (brglm) detect separation | Complete separation presents
library(detectseparation)
m1_sep <- glm(Kernel.color ~ ., data = foo, family = "binomial", method = "detect_separation")
m1_sep
#### 4) Suyoung's version of glmdr which uses nloptr | Complete separation presents
m1_glmdr <- glmdr(Kernel.color ~ + ., data = foo, family = "binomial")
summary(m1_glmdr)[[1]]
sum(!m1_glmdr$linearity) #74 problematic points
#### USING SUBSET i.e. EXCLUDE problematic points
#### 1) GLM
m2_glm <- glm(Kernel.color ~ ., data = foo[m1_glmdr$linearity,], family = "binomial")
summary(m2_glm)
#### 2) send glm to the infinity | cannot determine the complete separation
m2_inf <- glm(Kernel.color ~., data = foo[m1_glmdr$linearity,], family = "binomial"(link="logit"), control = list(maxit=10000,epsilon=1e-50))
m2_inf
sapply(foo[m1_glmdr$linearity,],table) #unlist(sapply(foo[m1_glmdr$linearity,],table))
#### 3) (brglm) detect separation | Complete separation does not present
m2_sep <- glm(Kernel.color ~ ., data = foo[m1_glmdr$linearity,], family = "binomial", method = "detect_separation")
m2_sep
#### 4) Suyoung's version of glmdr which uses nloptr | Complete separation does not present
m2_glmdr <- glmdr(Kernel.color ~ + ., data = foo[m1_glmdr$linearity,], family = "binomial")
summary(m2_glmdr)[[1]]
