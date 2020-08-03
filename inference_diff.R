b_obj_cond_chker <- function(beta,k,nulls,modmat,linearity){
  #Input:  model parameters
  #Output: True / False
  #Description: In logistic regression, check the necessary conditions for the objective function in auglag.
  ifelse(is.numeric(beta),TRUE,FALSE) |ifelse(is.finite(beta),TRUE,FALSE) |
    ifelse(length(beta) == ncol(nulls),TRUE,FALSE) |ifelse(is.numeric(k),TRUE,FALSE) |
    ifelse(is.finite(k),TRUE,FALSE) | ifelse(length(k) == 1,TRUE,FALSE) |
    ifelse(as.integer(k) == k,TRUE,FALSE)| ifelse(k %in% 1:nrow(modmat),TRUE,FALSE) |
    ifelse(! linearity[k],TRUE,FALSE)
}
b_hin_cond_chker <- function(beta, alpha, nulls){
  #Input:  model parameters
  #Output: True / False
  #Description: In logistic regression, check the necessary conditions for the constraints function in auglag.
  ifelse(is.numeric(beta),TRUE,FALSE) | ifelse(is.finite(beta),TRUE,FALSE) |
    ifelse(length(beta) == ncol(nulls),TRUE,FALSE) | ifelse(is.numeric(alpha),TRUE,FALSE) |
    ifelse(length(alpha) == 1,TRUE,FALSE) |ifelse(0 < alpha && alpha < 1,TRUE,FALSE)
}
my_inference <- function(object, alpha = 0.05){
  #Input: glmdr object and alpha
  #Output: List of length 2.
  #List1:
  #Upper and Lower confidence intervals (CI) for glmdr object
  #List2:
  # beta\xi at the convergence for each point
  #Description: Calculating CI and beta\xis for the glmdr object. 
  #Currently we compute CI using only the non-problematic points (i.e. linearity = FALSE)
  linearity = object$linearity
  if(all(linearity == TRUE)){
    stop("MLE is not at infinity, use glm functionality for inferences.")
  }
  om <- object$om 
  family <- object$family
  modmat <- object$modmat[! linearity,]
  nulls <- object$nulls
  y <- object$y[!linearity]
  if(class(y) == "matrix"){
    y <- object$y[!linearity,]
  }
  p <- q <- ncol(modmat)
  n <- nrow(modmat)
  O_mat.constr<- NULL
  theta.hat.constr <- predict.glm(object$om)[!linearity]
  if(is.null(nulls)){
    O_mat.constr <- modmat %*% diag(p) #identity matrix p x p
  }
  else{
    q <- dim(object$nulls)[2]
    O_mat.constr <- matrix((modmat %*% nulls),ncol=q)
  }
  lcm_beta <- matrix(0, nrow=sum(!linearity), ncol=q)
  # For completely degenerate logistic regression 
  if(family == "binomial"){
    if(class(y) == "integer" || class(y) == "numeric"){
      f <- function(beta, k, ...) {
        stopifnot(b_obj_cond_chker(beta,k,nulls,modmat,linearity))
        beta <- cbind(as.vector(beta))
        theta <- theta.hat.constr + O_mat.constr %*% beta
        #theta <- modmat %*% beta
        ifelse(y == 1, theta, - theta)[k]
      }
      df <- function(beta, k, ...) {
        stopifnot(b_obj_cond_chker(beta,k,nulls,modmat,linearity))
        ifelse(y == 1, 1, -1)[k] * as.vector(O_mat.constr[k, ])
      }
      g <- function(beta, alpha, ...) {
        stopifnot(b_hin_cond_chker(beta,alpha,nulls))
        theta <- theta.hat.constr + O_mat.constr %*% beta
        logp <- ifelse(theta < 0, theta - log1p(exp(theta)), - log1p(exp(- theta)))
        logq <- ifelse(theta < 0, - log1p(exp(theta)), - theta - log1p(exp(- theta)))
        logpboundary <- ifelse(y == 1, logp, logq)
        sum(logpboundary) - log(alpha)
      }
      dg <- function(beta, alpha, ...) {
        stopifnot(b_hin_cond_chker(beta,alpha,nulls))
        beta <- cbind(as.vector(beta))
        theta <- theta.hat.constr + O_mat.constr %*% beta
        pp <- ifelse(theta < 0, exp(theta) / (1 + exp(theta)),1 / (1 + exp(- theta)))
        qq <- ifelse(theta < 0, 1 / (1 + exp(theta)),exp(- theta) / (1 + exp(- theta)))
        result <- ifelse(y == 1, qq, - pp)
        result %*% O_mat.constr
      }
      beta.start <- matrix(rep(0, q))
      bounds <- rep(NA_real_, length(y))
      for (i in seq(along = bounds)) {
        aout <- auglag(beta.start, f, df, g, dg,
                       control.outer = list(trace = FALSE),
                       k = i, alpha = alpha)
        if (aout$convergence %in% c(0,9)){
          bounds[i] <- aout$value
          lcm_beta[i,] <- aout$par
        } 
        else{
          stop("auglag does not converge")
        }
      }
      bounds <- ifelse(y == 1, bounds, - bounds)
      bounds.lower.theta <- ifelse(y == 1, bounds, -Inf)
      bounds.upper.theta <- ifelse(y == 1, Inf, bounds)
      bounds.lower.p <- 1 / (1 + exp(- bounds.lower.theta))
      bounds.upper.p <- 1 / (1 + exp(- bounds.upper.theta))
      return(list(data.frame(lower = bounds.lower.p, upper = bounds.upper.p), beta=lcm_beta))
    }
    if(class(y) == "matrix"){ ## Bradley-Terry model
      y.int <- y[, 1] # has to be transformed to the right format # of linearity x number of nulls
      max.rows <- apply(y, 1, sum)
      f <- function(xi, k, ...) {
        stopifnot(b_obj_cond_chker(xi,k,nulls,modmat,linearity))
        xi <- cbind(as.vector(xi))
        theta <- theta.hat.constr + O_mat.constr %*% xi
        ifelse(y.int == max.rows, theta, - theta)[k]
      }
      df <- function(xi, k, ...) {
        stopifnot(b_obj_cond_chker(xi,k,nulls,modmat,linearity))
        ifelse(y.int == max.rows, 1, -1)[k] * as.vector(O_mat.constr[k, ])
      }
      g <- function(xi, alpha, ...) {
        stopifnot(b_hin_cond_chker(xi,alpha,nulls))
        xi <- cbind(as.vector(xi))
        theta <- theta.hat.constr + O_mat.constr %*% xi
        logp <- ifelse(theta < 0, theta - log1p(exp(theta)), - log1p(exp(- theta)))
        logq <- ifelse(theta < 0, - log1p(exp(theta)), - theta - log1p(exp(- theta)))
        logpboundary <- y.int * logp + (max.rows - y.int) * logq
        sum(logpboundary) - log(alpha)
      }
      dg <- function(xi, alpha, ...) {
        stopifnot(b_hin_cond_chker(xi,alpha,nulls))
        xi <- cbind(as.vector(xi))
        theta <- theta.hat.constr + O_mat.constr %*% xi
        pp <- ifelse(theta < 0, exp(theta) / (1 + exp(theta)), 1 / (1 + exp(- theta)))
        qq <- ifelse(theta < 0, 1 / (1 + exp(theta)), exp(- theta) / (1 + exp(- theta)))
        result <- ifelse(y.int < max.rows, y.int - max.rows * pp, max.rows * qq)
        result %*% O_mat.constr
      }
      xi.start <- rep(0, ncol(q))
      bounds <- rep(NA_real_, length(y.int))
      for (i in seq(along = bounds)){
          aout <- auglag(xi.start, f, df, g, dg,
                         control.outer = list(trace = FALSE),
                         k = i, alpha = alpha)
          if (aout$convergence %in% c(0,9)){
            bounds[i] <- aout$value
            lcm_beta[i,] <- aout$par
          }
          else{
            stop("auglag does not converge")
          }
        }
      bounds <- ifelse(y.int == max.rows, bounds, - bounds)
      bounds.lower.theta <- ifelse(y.int == 0, -Inf, bounds)
      bounds.upper.theta <- ifelse(y.int == max.rows, Inf, bounds)
      bounds.lower.p <- 1 / (1 + exp(- bounds.lower.theta))
      bounds.upper.p <- 1 / (1 + exp(- bounds.upper.theta))
      return(list(data.frame(lower = max.rows * bounds.lower.p, upper = max.rows * bounds.upper.p),lcm_beta))
    }
  }
}