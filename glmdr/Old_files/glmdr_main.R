###Library
library(glmdr) 
library(alabama) #for auglag
library(arm) #bayesglm method
library(brglm2) #AS methods
###Helper
CI_productor <- function(object){
  #Input: model (output of bayesglm or AS methods)
  #Output: Lower & Upper confidence intervals (CI)
  #Description: Helper function for Compare_methods() function. Calculate the CI
  prd <- predict(object, type = "link", se.fit = TRUE) 
  lwr<- prd$fit - 1.96 * prd$se.fit 
  upr <- prd$fit + 1.96 * prd$se.fit 
  cbind(invlogit(lwr), invlogit(upr))
}
Compare_methods <- function(object, round_digit=4){
  #Input: model from glmdr
  #Output: List of estimate/statistics
  #List 1: Lower & Upper confidence intervals (CI) for glmdr, bayesglm, 
  #AS_mixed, AS_mean, AS_median, AS_MPL<Jeffreys, AS_correction
  #List 2: Coefficients and Deviance of each method  (if possible)
  #Description: Provide the estimate/statistics (CI, coefficients, deviance) from different methods.
  ret <- vector("list",2)
  #CI
  #glmdr
  glmdr_out1.infer <- my_inference(object) #[[1]] CI [[2]] beta at convergence
  glmdr_out1.CI <- glmdr_out1.infer[[1]]
  #Bayesglm
  bayesglm_out1 <- bayesglm(object$om$formula, family = family(object$om), data = object$om$data)
  bayesglm_out1.CI <- CI_productor(bayesglm_out1)
  #brglmFit
  AS_mixed <- update(object$om, method="brglmFit", type="AS_mixed")
  AS_mixed.CI <- CI_productor(AS_mixed)
  AS_mean <- update(object$om, method = "brglmFit", type = "AS_mean")
  AS_mean.CI <- CI_productor(AS_mean)
  AS_median <- update(object$om, method = "brglmFit", type = "AS_median")
  AS_median.CI <- CI_productor(AS_median)
  AS_MPL_Jeffreys <- update(object$om, method = "brglmFit", type = "MPL_Jeffreys")
  AS_MPL_Jeffreys.CI <- CI_productor(AS_MPL_Jeffreys)
  AS_correction <- update(object$om, method = "brglmFit", type = "correction")
  AS_correction.CI <- CI_productor(AS_correction)
  # total length of CIs
  glmdr_length <- sum(as.matrix(glmdr_out1.CI) %*% c(-1,1)) # glmdr
  bayesglm_length <- sum(as.matrix(bayesglm_out1.CI[!object$linearity,]) %*% c(-1,1))
  AS_mixed_length <- sum(as.matrix(AS_mixed.CI[!object$linearity,]) %*% c(-1,1)) 
  AS_mean_length <- sum(as.matrix(AS_mean.CI[!object$linearity,]) %*% c(-1,1)) 
  AS_median_length <- sum(as.matrix(AS_median.CI[!object$linearity,]) %*% c(-1,1))
  AS_MPL_Jeffreys_length <- sum(as.matrix(AS_MPL_Jeffreys.CI[!object$linearity,]) %*% c(-1,1)) 
  AS_correction_length <- sum(as.matrix(AS_correction.CI[!object$linearity,]) %*% c(-1,1)) 
  ret[[1]] <- cbind(c("glmdr","Bayesglm","AS_mixed","AS_mean","AS_median","AS_MPL_Jeffreys","AS_correction"),
        round(c(glmdr_length,bayesglm_length,AS_mixed_length,AS_mean_length,
                AS_median_length,AS_MPL_Jeffreys_length,AS_correction_length),round_digit))
  #Coefficients
  yy <- object$y
  xx <- object$modmat
  beta_hat <- object$lcm$coefficients
  if(all(round(glmdr_out1.infer[[2]][1],round_digit) == round(glmdr_out1.infer[[2]],round_digit)) == FALSE){
    #Check if all points converges at the same beta.
    ret[[2]] <- NULL
    return(ret)
  }
  beta_hat[which(is.na(beta_hat))] = glmdr_out1.infer[[2]][1,]
  tb <- round(rbind(object$om$coefficients,
                    object$lcm$coefficients,
                    beta_hat,
                    bayesglm_out1$coefficients,
                    AS_mixed$coefficients,
                    AS_mean$coefficients,
                    AS_median$coefficients,
                    AS_MPL_Jeffreys$coefficients,
                    AS_correction$coefficients),round_digit)
  Method <- c("OM","LCM","inference","bayesglm","AS_mixed","AS_mean","AS_median","AS_MPL_Jeffreys","AS_correction")
  row.names(tb) = NULL
  #Deviance
  logit.deviance <- function(y,x,beta){
    #Description: Calculate the deviance of the model
    theta <- x %*% beta
    pi1 <- (1/ (1+exp(-theta)))[y==1]
    pi2 <- 1/ (1+exp(theta))[!y==1]
    2*(sum( -log(pi1)) +sum(- log(pi2) ))
  }
  a00 <- logit.deviance(glmdr_out1$y, glmdr_out1$modmat,glmdr_out1$om$coefficients)#om
  a0 <- deviance(glmdr_out1$lcm)
  #a0: deviance of lcm is equal to that of om. although one (or more) of coef is NA.
  a<-logit.deviance(yy, xx,beta_hat) 
  b<-logit.deviance(bayesglm_out1$y, model.matrix(bayesglm_out1),bayesglm_out1$coefficients)#bayesglm
  c<-logit.deviance(yy, xx,AS_mixed$coefficients) 
  d<-logit.deviance(yy, xx,AS_mean$coefficients)
  e<-logit.deviance(yy, xx,AS_median$coefficients) 
  f<-logit.deviance(yy, xx,AS_MPL_Jeffreys$coefficients) 
  g<-logit.deviance(yy, xx,AS_correction$coefficients) 
  Devi <- round(c(a00,a0,a,b,c,d,e,f,g),round_digit*2)
  ret[[2]] <- cbind(Method,tb,"Deviance"=Devi)
  return(ret)
}
###EX1 - endometrial
data(endometrial)
glmdr_out1 <- glmdr(HG ~., family = "binomial", data = endometrial)
Compare_methods(glmdr_out1)
#EX2 - complete separation when LCM space = whole parameter space
x1 <- 1:30
y1 <- c(rep(0, 12), rep(1, 11), rep(0, 7))

#EX3 - Quasi separation 1 - randomly generated.
y<- c(0,0,0,0,1,1,1,1,1,1)
x1<-c(1,2,3,3,3,4,5,6,20,21)
x2<-c(3,0,-1,4,1,0,2,7,3,4)

#EX4 - Quasi separation 2 - Agrsti

x <- seq(10, 90, 10)
x <- x[x != 50]
y <- as.numeric(x > 50) 
x <- c(x, 50, 50)
y <- c(y, 0, 1)

#EX5 - complete separation when LCM space = whole parameter space
dat1 <- iris
dat1_sub <- iris[1:100,]
mod<- glmdr((as.numeric(dat1_sub$Species)-1)~ Petal.Width+Sepal.Width, family="binomial", data = dat1_sub) 
#inference(tmp) 
#my_inference(tmp)

#EX6 - another example of EX1, complete separation when LCM space != whole parameter space
# require separation library for the dataset.
# reference: https://gist.github.com/carlislerainey
# Dealing with Separation in Logistic Regression Models: http://www.carlislerainey.com/papers/separation.pdf
# library(separation)
# dat1 <- politics_and_need
# dat1$dem_governor <- 1 - dat1$gop_governor  # create dem. gov. indicator
# dat1$st_percent_uninsured <- rescale(dat1$percent_uninsured)  # standardize 
# f <- oppose_expansion ~ dem_governor + percent_favorable_aca + gop_leg +
#   st_percent_uninsured + bal2012 + multiplier + percent_nonwhite + percent_metro
# mod <- glmdr(f, data = dat1, family = "binomial")

#EX7 - Bradley Terry model
team.names <- c("ants", "beetles", "cows", "dogs", "egrets", "foxes", "gerbils", "hogs")
dat <- matrix(c(NA, 2, 2, 2, 2, 2, 2, 2, 0, NA,
                 1, 2, 2, 2, 2, 2, 0, 1, NA, 2, 1, 2, 2, 2, 0,
                 0, 0, NA, 1, 1, 2, 2, 0, 0, 1, 1, NA, 1, 2, 2,
                 0, 0, 0, 1, 1, NA, 2, 2, 0, 0, 0, 0, 0, 0, NA,
                 1, 0, 0, 0, 0, 0, 0, 1, NA), byrow = TRUE, nrow = 8)
wins <- data[upper.tri(data)]
team.plus <- row(data)[upper.tri(data)] 
team.minus <- col(data)[upper.tri(data)] 
modmat <- matrix(0, length(wins), nrow(data)) 
for (i in 1:ncol(modmat)) {
  modmat[team.plus == i, i] <- 1 
  modmat[team.minus == i, i] <- (-1)
}
losses <- 2 - wins
resp <- cbind(wins, losses)
colnames(modmat) <- team.names
sportsdata <- cbind(modmat, wins, losses) 
sportsdata <- as.data.frame(sportsdata)
mod <- glmdr(cbind(wins, losses) ~ 0 + ., family = "binomial", data = sportsdata)