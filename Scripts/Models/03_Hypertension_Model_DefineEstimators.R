#### Hypertension: Define Estimators ####
# Purpose: Define the double-bounded choice estimators and individual willingness-to-pay calculations.
# Author: Anonymised for peer review.
# Prerequisites: Restore packages with script 01.
# Run from the project root: source("Scripts/Models/03_Hypertension_Model_DefineEstimators.R", encoding = "UTF-8")
# Inputs:
#   Installed R packages; no survey data.
# Outputs:
#   Estimation functions in the R session; no files written.
# Software versions: renv.lock.


# *************************************************************
#### Section 0: Setup libraries and inputs ####
# *************************************************************


library(Formula)


library(DCchoice)


library(AER)


library(Ecdat)


library(Ecfun)


library(lmtest)


library(interval)


library(ggalt)


library(ggplot2)


library(dplyr)


library(RColorBrewer)


# *************************************************************
#### Section 1: #modified dbspike function that switches nny to 0 and nnn to 1 ####
# *************************************************************
# dbspike <- function (formula, data, subset, na.action = na.omit, dist ="logistic", par = NULL,...){
#   cl <- match.call()
#   # checking arguments
#   if(!inherits(formula, "Formula")) {
#     formula <- Formula(formula)
#   }
#   if(!inherits(formula, "formula")) {
#     stop("invalid formula")
#   }
#   if(length(formula[[2]]) != 3) {
#     stop("LHS variable in the formula must be like y1 + y2 ")
#   }
#   if(missing(data)) {
#     stop("data frame must be assigned to argument 'data'")
#   }
#   if(dist != "logistic" & dist != "log-logistic" & dist != "normal" & dist != "log-normal" & dist != "weibull"){
#     stop("Too bad, 'dist' is incorrectly specified :(")
#   }
#
#   # preparing a data set for analysis
#   mf <- match.call(expand.dots = TRUE)
#   m  <- match(c("formula", "data", "subset", "na.action"), names(mf), 0L)
#   mf <- mf[c(1L, m)]
#   mf$formula <- formula
#   mf[[1L]] <- as.name("model.frame")
#   mf <- eval(mf, parent.frame())
#   original.data <- data
#   data <- mf
#
#   # defining a dependent variable
#   y1  <- model.part(formula, data = data, lhs = 1)[[1]] # y/n to 1st bid
#   y2  <- model.part(formula, data = data, lhs = 1)[[2]] # y/n to 2nd bid
#   spk <- model.part(formula, data = data, lhs = 2)[[1]] # 2nd part of LHS(spike)
#   nobs <- length(y1)
#
#   if(is.factor(y1)){
#     yy <- ifelse(y1 == "yes" & y2 == "yes", 1, 0)
#     yn <- ifelse(y1 == "yes" & y2 == "no",  1, 0)
#     ny <- ifelse(y1 == "no"  & y2 == "yes", 1, 0)
#     nn <- ifelse(y1 == "no"  & y2 == "no",  1, 0)
#   }
#   else {
#     yy <- ifelse(y1 == 1 & y2 == 1, 1, 0)
#     yn <- ifelse(y1 == 1 & y2 == 0, 1, 0)
#     ny <- ifelse(y1 == 0 & y2 == 1, 1, 0)
#     nn <- ifelse(y1 == 0 & y2 == 0, 1, 0)
#   }
#   yvar <- cbind(yy, yn, ny, nn, spk)
#
#   # creating a design matrix
#   bid <- model.part(formula, data, lhs = 0, rhs = 2)
#   BID <- ifelse(bid[, 1] > bid[, 2], bid[, 2], bid[, 1])
#
#   X     <- model.part(formula, data, lhs = 0, rhs = 1)
#   mmX <- model.matrix(formula, data, lhs = 0, rhs = 1)
#
#   if(!any(colnames(mmX) == "(Intercept)")) {
#     stop(message = "constant (intercept) term is required for the formula")
#   }
#
#   # obtaining initial parameter values
#   tmp.data <- data.frame(y1, mmX, BID)
#
#   if(is.null(par)){
#     f.stage <- glm(y1~. -1, family = binomial(link = "probit"), data = tmp.data)
#     ini <- f.stage$coefficients # saving initial values for ML estimation
#     npar <- length(ini)
#     ini[npar] <- ifelse(ini[npar] > 0, -ini[npar], ini[npar])     # gives a negative initial value for the bid coefficient
#     if (substr(dist, 1, 4) == "log-" | dist=="weibull") names(ini)[npar] <- "log(BID)"
#     names(ini)[1] <- "(Intercept)"
#   }
#   else { # initial parameter values are supplied by the user
#     if(length(par) != ncol(tmp.data)-1)
#       stop(paste0("Sorry - length of par is wrong again:(\nDetails are  length(par): ", length(par), " and ncol(tmp.data)-1: ", ncol(tmp.data)-1))
#     # stop("Sorry mate - length of 'par' should be ncol(data.frame(y1, mmX, BID))-1 (number of explanatory variables.)")
#     ini <- par
#     f.stage <- ini
#   }
#
#   if(dist == "logistic"|dist == "log-logistic"){
#     # likelihood function
#     dbLL <- function(param, dvar, ivar, bid){
#       yy  <- dvar[, 1]
#       yn  <- dvar[, 2]
#       ny  <- dvar[, 3]
#       nn  <- dvar[, 4]
#       spk <- dvar[, 5]
#       nny <- nn * (spk == 0)
#       nnn <- nn * (spk == 1)
#       X1  <- cbind(ivar, bid[, 1])
#       X2  <- cbind(ivar, bid[, 2])
#       X0  <- X1
#       X0[, ncol(X0)] <- 0
#
#       ll <- sum(log(plogis(-X2[yy  == 1, , drop = FALSE] %*% param,lower.tail = FALSE, log.p = FALSE))) + # y-y
#         sum(log(plogis(-X2[yn  == 1, , drop = FALSE] %*% param,lower.tail =  TRUE, log.p = FALSE) -   # y-n (1)
#                   plogis(-X1[yn  == 1, , drop = FALSE] %*% param,lower.tail =  TRUE, log.p = FALSE))) + # y-n (2)
#         sum(log(plogis(-X1[ny  == 1, , drop = FALSE] %*% param,lower.tail =  TRUE, log.p = FALSE) -   # n-y (1)
#                   plogis(-X2[ny  == 1, , drop = FALSE] %*% param,lower.tail =  TRUE, log.p = FALSE))) + # n-y (2)
#         sum(log(plogis(-X2[nny == 1, , drop = FALSE] %*% param,lower.tail =  TRUE, log.p = FALSE) -   # n-n-y (1)
#                   plogis(-X0[nny == 1, , drop = FALSE] %*% param,lower.tail =  TRUE, log.p = FALSE))) + # n-n-y (2)
#         sum(log(plogis(-X0[nnn == 1, , drop = FALSE] %*% param,lower.tail =  TRUE, log.p = FALSE)))   # n-n-n
#       ifelse(is.finite(ll), return(-ll), NaN)
#     }
#   }
#   else if(dist == "weibull"){
#     dbLL <- function(param, dvar, ivar, bid){
#       yy  <- dvar[, 1]
#       yn  <- dvar[, 2]
#       ny  <- dvar[, 3]
#       nn  <- dvar[, 4]
#       spk <- dvar[, 5]
#       nny <- nn * (spk == 0)
#       nnn <- nn * (spk == 1)
#       X1  <- cbind(ivar, bid[, 1])
#       X2  <- cbind(ivar, bid[, 2])
#       X0  <- X1
#       X0[, ncol(X0)] <- 0
#
#       ll <- sum(log(pweibull(exp(-X2[yy  == 1, ,drop = FALSE] %*% param),shape=1,lower.tail = FALSE, log.p = FALSE))) + # y-y
#         sum(log(pweibull(exp(-X2[yn  == 1, ,drop = FALSE] %*% param),shape=1,lower.tail =  TRUE, log.p = FALSE) -   # y-n (1)
#                   pweibull(exp(-X1[yn  == 1, ,drop = FALSE] %*% param),shape=1,lower.tail =  TRUE, log.p = FALSE))) + # y-n (2)
#         sum(log(pweibull(exp(-X1[ny  == 1, ,drop = FALSE] %*% param),shape=1,lower.tail =  TRUE, log.p = FALSE) -   # n-y (1)
#                   pweibull(exp(-X2[ny  == 1, ,drop = FALSE] %*% param),shape=1,lower.tail =  TRUE, log.p = FALSE))) + # n-y (2)
#         sum(log(pweibull(exp(-X2[nny == 1, ,drop = FALSE] %*% param),shape=1,lower.tail =  TRUE, log.p = FALSE) -   # n-n-y (1)
#                   pweibull(exp(-X0[nny == 1, ,drop = FALSE] %*% param),shape=1,lower.tail =  TRUE, log.p = FALSE))) + # n-n-y (2)
#         sum(log(pweibull(exp(-X0[nnn == 1, ,drop = FALSE] %*% param),shape=1,lower.tail =  TRUE, log.p = FALSE)))   # n-n-n
#       ifelse(is.finite(ll), return(-ll), NaN)
#     }
#   }
#   else if(dist == "normal"|dist == "log-normal"){
#     # likelihood function
#     dbLL <- function(param, dvar, ivar, bid){
#       yy  <- dvar[, 1]
#       yn  <- dvar[, 2]
#       ny  <- dvar[, 3]
#       nn  <- dvar[, 4]
#       spk <- dvar[, 5]
#       nny <- nn * (spk == 0)
#       nnn <- nn * (spk == 1)
#       X1  <- cbind(ivar, bid[, 1])
#       X2  <- cbind(ivar, bid[, 2])
#       X0  <- X1
#       X0[, ncol(X0)] <- 0
#
#       ll <- sum(log(pnorm(-X2[yy  == 1, , drop = FALSE] %*% param,lower.tail = FALSE, log.p = FALSE))) + # y-y
#         sum(log(pnorm(-X2[yn  == 1, , drop = FALSE] %*% param,lower.tail =  TRUE, log.p = FALSE) -   # y-n (1)
#                   pnorm(-X1[yn  == 1, , drop = FALSE] %*% param,lower.tail =  TRUE, log.p = FALSE))) + # y-n (2)
#         sum(log(pnorm(-X1[ny  == 1, , drop = FALSE] %*% param,lower.tail =  TRUE, log.p = FALSE) -   # n-y (1)
#                   pnorm(-X2[ny  == 1, , drop = FALSE] %*% param,lower.tail =  TRUE, log.p = FALSE))) + # n-y (2)
#         sum(log(pnorm(-X2[nny == 1, , drop = FALSE] %*% param,lower.tail =  TRUE, log.p = FALSE) -   # n-n-y (1)
#                   pnorm(-X0[nny == 1, , drop = FALSE] %*% param,lower.tail =  TRUE, log.p = FALSE))) + # n-n-y (2)
#         sum(log(pnorm(-X0[nnn == 1, , drop = FALSE] %*% param,lower.tail =  TRUE, log.p = FALSE)))   # n-n-n
#       ifelse(is.finite(ll), return(-ll), NaN)
#     }
#   }
#
#   # estimating DBDC spike model
#   suppressWarnings(
#     dbspike <- optim(ini,
#                      fn = dbLL,
#                      method = "SANN",# normally BFGS, might sometimes be switched to "SANN" for the purpose of finding starting values
#                      hessian = T,
#                      dvar = yvar,
#                      ivar = mmX,
#                      bid = bid,
#                      control = list(abstol = 10^(-30))))
#   npar <- length(dbspike$par)
#   terms <- terms(formula)
#
#   # storing factor levels
#   fac <- which(sapply(X, is.factor) == TRUE)
#   xlevels <- as.list(fac)
#   j <- 0
#   for (i in fac) {
#     j <- j + 1
#     xlevels[[j]] <- levels(X[[i]])
#   }
#
#   # arranging outcomes into a list object
#   output <- list(
#     f.stage = f.stage,          # the outcome of the initial estimation
#     dbspike  = dbspike,         # the outcome from the optimization
#     coefficients = dbspike$par, # the coefficient estimates
#     call = cl,                  # the function call
#     formula = formula,          # the defined model formula
#     Hessian = dbspike$hessian,  # the numerical Hessian at the estimates
#     distribution = dist,        # the specified error distribution
#     loglik = -dbspike$value,    # log-likelihood at the estimates
#     convergence = ifelse(dbspike$convergence == 0, TRUE, FALSE),   # convergence status
#     niter = dbspike$counts,     # the number of iterations
#     nobs = nobs,                # the number of observations
#     covariates = mmX,           # a matrix of the covariates
#     bid = bid,                  # the suggested bid
#     yn = cbind(y1, y2),         # the acceptance/rejection variable
#     data.name = data,           # the data matrix
#     spk = spk,
#     terms = terms,
#     contrasts = contrasts,
#     data = original.data,
#     xlevels = xlevels)
#
#   # setting the object class
#   class(output) <- c("spike", "dbspike")
#
#   return(output)
# }#


## Estimate with BFGS; use simulated annealing (SANN) if needed.
dbspike <- function(formula, data, subset, na.action = na.omit, dist = "logistic", par = NULL, ...) {
  cl <- match.call()

  # Input validation
  if (!inherits(formula, "Formula")) {
    formula <- Formula(formula)
  }
  if (!inherits(formula, "formula")) {
    stop("invalid formula")
  }
  if (length(formula[[2]]) != 3) {
    stop("LHS variable in the formula must be like y1 + y2 ")
  }
  if (missing(data)) {
    stop("data frame must be assigned to argument 'data'")
  }

  valid_distributions <- c("logistic", "log-logistic", "normal", "log-normal", "weibull")
  if (!(dist %in% valid_distributions)) {
    stop("Invalid 'dist' specification. Must be one of: ", paste(valid_distributions, collapse = ", "))
  }

  # Prepare data for analysis
  mf <- match.call(expand.dots = TRUE)
  m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$formula <- formula
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  original.data <- data
  data <- mf

  # Extract dependent variables
  y1 <- model.part(formula, data = data, lhs = 1)[[1]] # y/n to 1st bid
  y2 <- model.part(formula, data = data, lhs = 1)[[2]] # y/n to 2nd bid
  spk <- model.part(formula, data = data, lhs = 2)[[1]] # 2nd part of LHS(spike)
  nobs <- length(y1)

  # Create response indicators
  if (is.factor(y1)) {
    yy <- y1 == "yes" & y2 == "yes"
    yn <- y1 == "yes" & y2 == "no"
    ny <- y1 == "no"  & y2 == "yes"
    nn <- y1 == "no"  & y2 == "no"
  } else {
    yy <- y1 == 1 & y2 == 1
    yn <- y1 == 1 & y2 == 0
    ny <- y1 == 0 & y2 == 1
    nn <- y1 == 0 & y2 == 0
  }

  # Create dummy variables
  nny <- ifelse(nn == 1 & spk == 0, 1, 0)
  nnn <- ifelse(nn == 1 & spk == 1, 1, 0)

  # Store responses for likelihood calculation
  yvar <- cbind(yy, yn, ny, nn, spk)

  # Create design matrix
  bid <- model.part(formula, data, lhs = 0, rhs = 2)
  BID <- ifelse(bid[, 1] > bid[, 2], bid[, 2], bid[, 1])
  X <- model.part(formula, data, lhs = 0, rhs = 1)
  mmX <- model.matrix(formula, data, lhs = 0, rhs = 1)

  if (!any(colnames(mmX) == "(Intercept)")) {
    stop("constant (intercept) term is required for the formula")
  }

  # Get initial parameter values
  if (is.null(par)) {
    tmp.data <- data.frame(y1, mmX, BID)
    f.stage <- glm(y1 ~ . - 1, family = binomial(link = "probit"), data = tmp.data)
    ini <- f.stage$coefficients
    npar <- length(ini)
    # Ensure negative value for bid coefficient
    ini[npar] <- if(ini[npar] > 0) -ini[npar] else ini[npar]

    if (substr(dist, 1, 4) == "log-" || dist == "weibull") {
      names(ini)[npar] <- "log(BID)"
    }
    names(ini)[1] <- "(Intercept)"
  } else {
    # Check if user-provided parameters match expected length
    tmp.data <- data.frame(y1, mmX, BID)
    expected_length <- ncol(tmp.data) - 1
    if (length(par) != expected_length) {
      stop(sprintf("Parameter length mismatch: got %d, expected %d", length(par), expected_length))
    }
    ini <- par
    f.stage <- ini  # Store initial values
  }

  # Define likelihood functions based on distribution
  if (dist == "logistic"|dist == "log-logistic") {
    # likelihood function
    dbLL <- function(param, dvar, ivar, bid) {
      yy  <- dvar[, 1]
      yn  <- dvar[, 2]
      ny  <- dvar[, 3]
      nn  <- dvar[, 4]
      spk <- dvar[, 5]
      nny <- nn * (spk == 0)
      nnn <- nn * (spk == 1)
      X1  <- cbind(ivar, bid[, 1])
      X2  <- cbind(ivar, bid[, 2])
      X0  <- X1
      X0[, ncol(X0)] <- 0

      ll <- sum(log(plogis(-X2[yy  == 1, , drop = FALSE] %*% param, lower.tail = FALSE, log.p = FALSE))) + # y-y
        sum(log(plogis(-X2[yn  == 1, , drop = FALSE] %*% param, lower.tail =  TRUE, log.p = FALSE) -   # y-n (1)
                  plogis(-X1[yn  == 1, , drop = FALSE] %*% param, lower.tail =  TRUE, log.p = FALSE))) + # y-n (2)
        sum(log(plogis(-X1[ny  == 1, , drop = FALSE] %*% param, lower.tail =  TRUE, log.p = FALSE) -   # n-y (1)
                  plogis(-X2[ny  == 1, , drop = FALSE] %*% param, lower.tail =  TRUE, log.p = FALSE))) + # n-y (2)
        sum(log(plogis(-X2[nny == 1, , drop = FALSE] %*% param, lower.tail =  TRUE, log.p = FALSE) -   # n-n-y (1)
                  plogis(-X0[nny == 1, , drop = FALSE] %*% param, lower.tail =  TRUE, log.p = FALSE))) + # n-n-y (2)
        sum(log(plogis(-X0[nnn == 1, , drop = FALSE] %*% param, lower.tail =  TRUE, log.p = FALSE)))   # n-n-n

      ifelse(is.finite(ll), return(-ll), NaN)
    }
  }
  else if (dist == "weibull") {
    dbLL <- function(param, dvar, ivar, bid) {
      yy  <- dvar[, 1]
      yn  <- dvar[, 2]
      ny  <- dvar[, 3]
      nn  <- dvar[, 4]
      spk <- dvar[, 5]
      nny <- nn * (spk == 0)
      nnn <- nn * (spk == 1)
      X1  <- cbind(ivar, bid[, 1])
      X2  <- cbind(ivar, bid[, 2])
      X0  <- X1
      X0[, ncol(X0)] <- 0

      ll <- sum(log(pweibull(exp(-X2[yy  == 1, , drop = FALSE] %*% param), shape = 1, lower.tail = FALSE, log.p = FALSE))) + # y-y
        sum(log(pweibull(exp(-X2[yn  == 1, , drop = FALSE] %*% param), shape = 1, lower.tail =  TRUE, log.p = FALSE) -   # y-n (1)
                  pweibull(exp(-X1[yn  == 1, , drop = FALSE] %*% param), shape = 1, lower.tail =  TRUE, log.p = FALSE))) + # y-n (2)
        sum(log(pweibull(exp(-X1[ny  == 1, , drop = FALSE] %*% param), shape = 1, lower.tail =  TRUE, log.p = FALSE) -   # n-y (1)
                  pweibull(exp(-X2[ny  == 1, , drop = FALSE] %*% param), shape = 1, lower.tail =  TRUE, log.p = FALSE))) + # n-y (2)
        sum(log(pweibull(exp(-X2[nny == 1, , drop = FALSE] %*% param), shape = 1, lower.tail =  TRUE, log.p = FALSE) -   # n-n-y (1)
                  pweibull(exp(-X0[nny == 1, , drop = FALSE] %*% param), shape = 1, lower.tail =  TRUE, log.p = FALSE))) + # n-n-y (2)
        sum(log(pweibull(exp(-X0[nnn == 1, , drop = FALSE] %*% param), shape = 1, lower.tail =  TRUE, log.p = FALSE)))   # n-n-n

      ifelse(is.finite(ll), return(-ll), NaN)
    }
  }
  else if (dist == "normal"|dist == "log-normal") {
    # likelihood function
    dbLL <- function(param, dvar, ivar, bid) {
      yy  <- dvar[, 1]
      yn  <- dvar[, 2]
      ny  <- dvar[, 3]
      nn  <- dvar[, 4]
      spk <- dvar[, 5]
      nny <- nn * (spk == 0)
      nnn <- nn * (spk == 1)
      X1  <- cbind(ivar, bid[, 1])
      X2  <- cbind(ivar, bid[, 2])
      X0  <- X1
      X0[, ncol(X0)] <- 0

      ll <- sum(log(pnorm(-X2[yy  == 1, , drop = FALSE] %*% param, lower.tail = FALSE, log.p = FALSE))) + # y-y
        sum(log(pnorm(-X2[yn  == 1, , drop = FALSE] %*% param, lower.tail =  TRUE, log.p = FALSE) -   # y-n (1)
                  pnorm(-X1[yn  == 1, , drop = FALSE] %*% param, lower.tail =  TRUE, log.p = FALSE))) + # y-n (2)
        sum(log(pnorm(-X1[ny  == 1, , drop = FALSE] %*% param, lower.tail =  TRUE, log.p = FALSE) -   # n-y (1)
                  pnorm(-X2[ny  == 1, , drop = FALSE] %*% param, lower.tail =  TRUE, log.p = FALSE))) + # n-y (2)
        sum(log(pnorm(-X2[nny == 1, , drop = FALSE] %*% param, lower.tail =  TRUE, log.p = FALSE) -   # n-n-y (1)
                  pnorm(-X0[nny == 1, , drop = FALSE] %*% param, lower.tail =  TRUE, log.p = FALSE))) + # n-n-y (2)
        sum(log(pnorm(-X0[nnn == 1, , drop = FALSE] %*% param, lower.tail =  TRUE, log.p = FALSE)))   # n-n-n

      ifelse(is.finite(ll), return(-ll), NaN)
    }
  }

  # Estimate DBDC spike model
  # Modified optimisation section
  suppressWarnings(
    dbspike <- optim(ini,
                     fn = dbLL,
                     method = "BFGS", # Use the parameter
                     hessian = T,
                     dvar = yvar,
                     ivar = mmX,
                     bid = bid,
                     control = list(abstol = 10^(-30))))

  # Check convergence and retry with SANN if BFGS failed and we're using Weibull
  if(dbspike$convergence != 0 && dist == "weibull" && optim_method == "BFGS") {
    warning("BFGS optimisation failed to converge for Weibull distribution. Trying SANN...")
    suppressWarnings(
      dbspike_sann <- optim(ini,
                            fn = dbLL,
                            method = "SANN",
                            hessian = F, # No Hessian in first SANN run
                            dvar = yvar,
                            ivar = mmX,
                            bid = bid,
                            control = list(abstol = 10^(-30))))

    # Use SANN results as starting values for BFGS
    suppressWarnings(
      dbspike <- optim(dbspike_sann$par,
                       fn = dbLL,
                       method = "BFGS",
                       hessian = T,
                       dvar = yvar,
                       ivar = mmX,
                       bid = bid,
                       control = list(abstol = 10^(-30))))
  }

  npar <- length(dbspike$par)
  terms <- terms(formula)

  # Store factor levels
  fac <- which(sapply(X, is.factor) == TRUE)
  xlevels <- as.list(fac)
  j <- 0
  for (i in fac) {
    j <- j + 1
    xlevels[[j]] <- levels(X[[i]])
  }

  # Create output list
  output <- list(
    f.stage = f.stage,          # the outcome of the initial estimation
    dbspike  = dbspike,         # the outcome from the optimization
    coefficients = dbspike$par, # the coefficient estimates
    call = cl,                  # the function call
    formula = formula,          # the defined model formula
    Hessian = dbspike$hessian,  # the numerical Hessian at the estimates
    distribution = dist,        # the specified error distribution
    loglik = -dbspike$value,    # log-likelihood at the estimates
    convergence = ifelse(dbspike$convergence == 0, TRUE, FALSE),   # convergence status
    niter = dbspike$counts,     # the number of iterations
    nobs = nobs,                # the number of observations
    covariates = mmX,           # a matrix of the covariates
    bid = bid,                  # the suggested bid
    yn = cbind(y1, y2),         # the acceptance/rejection variable
    data.name = data,           # the data matrix
    spk = spk,
    terms = terms,
    contrasts = contrasts,
    data = original.data,
    xlevels = xlevels
  )

  # Set object class
  class(output) <- c("spike", "dbspike")
  return(output)
}


summary.spike <- function(object, ...) {

  # --- 1. Extract Basic Information ---
  if (!inherits(object, "spike")) stop("Input 'object' must be of class 'spike'.")

  coef <- object$coefficients
  if (is.null(coef)) stop("object$coefficients not found.")
  npar <- length(coef)

  dist <- object$distribution
  if (is.null(dist)) stop("object$distribution not found.")

  loglik_full <- object$loglik
  if (is.null(loglik_full) || !is.finite(loglik_full)) {
    warning("Full model log-likelihood (object$loglik) is missing or non-finite.")
    # Decide how to proceed: stop? Set to NA? Depends on requirements.
    # For now, allow potential NA propagation.
  }

  nobs <- object$nobs
  if (is.null(nobs)) stop("object$nobs not found.")


  # --- 2. Calculate Standard Errors ---
  se <- NULL # Initialize se
  vcov_matrix <- tryCatch(vcov(object), error = function(e) NULL)

  if (!is.null(vcov_matrix) && is.matrix(vcov_matrix) && all(dim(vcov_matrix) == c(npar, npar))) {
    # Method 1: Use vcov() if available and valid
    diag_vcov <- diag(vcov_matrix)
    if (any(diag_vcov < 0)) {
      warning("Negative variances found in vcov matrix. Taking sqrt of absolute value.")
      se <- sqrt(abs(diag_vcov))
    } else {
      se <- sqrt(diag_vcov)
    }
    message("Standard errors calculated using vcov().")

  } else {
    # Method 2: Try using Hessian matrix
    warning("vcov(object) failed or returned invalid matrix. Attempting calculation via Hessian.")
    hessian_matrix <- object$Hessian
    if (!is.null(hessian_matrix) && is.matrix(hessian_matrix) && all(dim(hessian_matrix) == c(npar, npar))) {

      # Try Cholesky decomposition inversion (numerically preferred)
      hess_chol <- tryCatch(chol(hessian_matrix), error = function(e) NULL)
      if (!is.null(hess_chol)) {
        inv_hessian <- tryCatch(chol2inv(hess_chol), error = function(e) NULL)
        if (!is.null(inv_hessian)) {
          diag_inv_hess <- diag(inv_hessian)
          if (any(diag_inv_hess < 0)) {
            warning("Negative variances found after Hessian inversion (Cholesky). Taking sqrt of absolute value.")
            se <- sqrt(abs(diag_inv_hess))
          } else {
            se <- sqrt(diag_inv_hess)
          }
          message("Standard errors calculated using chol2inv(chol(Hessian)).")
        }
      }

      # Fallback to solve() if Cholesky failed or chol2inv failed
      if (is.null(se)) {
        warning("Cholesky decomposition/inversion failed. Falling back to solve(Hessian).")
        inv_hessian_solve <- tryCatch(solve(hessian_matrix), error = function(e) NULL)
        if (!is.null(inv_hessian_solve)) {
          diag_inv_hess_solve <- diag(inv_hessian_solve)
          if (any(diag_inv_hess_solve < 0)) {
            warning("Negative variances found after Hessian inversion (solve). Taking sqrt of absolute value.")
            se <- sqrt(abs(diag_inv_hess_solve))
          } else {
            se <- sqrt(diag_inv_hess_solve)
          }
          message("Standard errors calculated using solve(Hessian).")
        } else {
          warning("solve(Hessian) also failed.")
        }
      }

    } else {
      warning("object$Hessian is missing, not a matrix, or has incorrect dimensions.")
    }
  }

  # Final check for SEs
  if (is.null(se) || length(se) != npar) {
    warning("Failed to calculate standard errors. Setting SEs to NA.")
    se <- rep(NA_real_, npar)
  }


  # --- 3. Fit Null Model (if necessary) ---
  loglik_null <- object$loglik_null
  npar_null   <- object$npar_null

  refit_null_model <- TRUE
  if (!is.null(loglik_null) && is.finite(loglik_null) &&
      !is.null(npar_null) && is.finite(npar_null) && npar_null > 0 && npar_null < npar) {
    message("Using pre-calculated null model log-likelihood and npar.")
    refit_null_model <- FALSE
  }

  if (refit_null_model) {
    message("Null model information missing or invalid. Refitting null model...")

    # --- 3a. Get Data for Null Model ---
    # ... (same logic as before: try object$model_frame, fallback to object$call$data) ...
    if (!is.null(object$model_frame) && is.data.frame(object$model_frame)) {
      data_for_null <- object$model_frame
    } else {
      warning("Stored model frame not found in object$model_frame. Trying to evaluate object$call$data.")
      data_call_arg <- object$call$data
      if(!is.null(data_call_arg)) {
        eval_env <- environment(object$formula); if (is.null(eval_env)) eval_env <- parent.frame()
        data_for_null <- tryCatch(eval(data_call_arg, envir = eval_env), error = function(e) NULL)
      } else { data_for_null <- NULL }
      if(is.null(data_for_null) || !is.data.frame(data_for_null)) {
        stop("Cannot find or evaluate data for null model fitting (checked object$model_frame and object$call$data). Required for refitting null model.")
      }
    }

    # --- 3b. Construct Null Formula (Revised Method) ---
    if (!requireNamespace("Formula", quietly = TRUE)) stop("Package 'Formula' is required but not installed.")

    current_formula_obj <- tryCatch(
      Formula::Formula(object$formula),
      error = function(e) stop("object$formula is missing or cannot be converted to a Formula object: ", e$message)
    )

    # Extract LHS and RHS parts as separate formula objects
    formula_lhs_part <- tryCatch(
      formula(current_formula_obj, lhs = 1, rhs = 0),
      error = function(e) stop("Failed to extract LHS part from formula: ", e$message)
    )
    formula_rhs2_part <- tryCatch(
      formula(current_formula_obj, lhs = 0, rhs = 2),
      error = function(e) stop("Failed to extract RHS part 2 (bids) from formula: ", e$message)
    )

    # Check if extraction resulted in valid formula objects
    if (!inherits(formula_lhs_part, "formula") || length(formula_lhs_part) != 2) {
      stop("Extracted LHS part is not a valid formula structure (~ expression).")
    }
    if (!inherits(formula_rhs2_part, "formula") || length(formula_rhs2_part) != 2) {
      stop("Extracted RHS part 2 (bids) is not a valid formula structure (~ expression).")
    }

    # Assemble the null formula using the extracted formulas
    formula_null <- tryCatch(
      Formula::as.Formula(formula_lhs_part, ~ 1, formula_rhs2_part),
      error = function(e) stop("Failed to assemble null formula using Formula::as.Formula: ", e$message)
    )

    # --- 3c. Get Initial Parameters for Null Model ---
    # Assuming intercept is first, bid parameter is last in the full model 'coef' vector
    if (npar < 2) stop("Cannot determine initial parameters for null model (requires at least intercept and bid coeff in full model).")
    # Use estimates from the full model as starting points for intercept and bid coeff
    null_start_par <- coef[c(1, npar)]

    # --- 3d. Determine and Call the Fitting Function ---
    # Assume the corresponding *_new function exists if summary is called on a new object
    fitter_func_name <- paste0(class(object)[2], "_new") # e.g., "dbspike_new"
    if (!exists(fitter_func_name, mode = "function")) {
      # Fallback to original name if *_new doesn't exist
      fitter_func_name <- class(object)[2]
      if (!exists(fitter_func_name, mode = "function")) {
        stop(paste("Cannot find fitting function:", fitter_func_name, "or", paste0(fitter_func_name, "_new")))
      } else {
        warning(paste("Using original fitting function", fitter_func_name, "for null model."))
      }
    }
    fitter_func <- get(fitter_func_name, mode = "function")

    # --- 3e. Fit the Null Model ---
    null_model <- tryCatch(
      fitter_func(
        formula = formula_null,
        data = data_for_null,
        dist = dist,
        par = null_start_par,
        optim_method = "BFGS" # Use BFGS for null fit speed? Or match object$call? Safer to use default BFGS.
        # Add other relevant arguments passed via ... if needed? Usually not for null.
      ),
      error = function(e) {
        warning("Failed to fit the null model: ", e$message)
        NULL # Return NULL if fitting crashes
      }
    )

    # --- 3f. Extract Results from Null Model ---
    if (is.null(null_model) || null_model$convergence == FALSE) {
      warning("Null model fitting failed or did not converge. LR test results will be NA.")
      loglik_null <- NA
      npar_null <- NA # Unknown number of parameters if failed
    } else {
      loglik_null <- null_model$loglik
      npar_null <- length(null_model$coefficients)

      # Optionally store these back into the object?
      # object$loglik_null <- loglik_null
      # object$npar_null <- npar_null
    }
  } # End of refit_null_model block

  # --- 4. Calculate AIC and BIC ---
  if(is.finite(loglik_full)) {
    object$AIC <- c(
      AIC = -2 * loglik_full + 2 * npar,
      BIC = -2 * loglik_full + log(nobs) * npar
    )
  } else {
    object$AIC <- c(AIC = NA, BIC = NA)
  }

  # --- 5. Calculate Likelihood Ratio Test ---
  LR <- NA; d.f <- NA; pvalLR <- NA # Initialize
  if (is.finite(loglik_full) && is.finite(loglik_null) && is.finite(npar_null) && npar > npar_null) {
    LR <- 2 * (loglik_full - loglik_null)
    d.f <- npar - npar_null

    if (LR < 0 && abs(LR) < sqrt(.Machine$double.eps)) {
      # Handle tiny numerical inaccuracies resulting in slightly negative LR
      warning(paste("LR statistic is slightly negative (", signif(LR, 4), "), treating as zero.", sep=""))
      LR <- 0
      pvalLR <- 1.0
    } else if (LR < 0) {
      # More significant negative LR indicates a problem
      warning(paste("LR statistic is negative (", signif(LR, 4), "). Check log-likelihoods or model convergence.", sep=""))
      pvalLR <- NA
    } else {
      pvalLR <- pchisq(LR, df = d.f, lower.tail = FALSE)
    }
  } else {
    if (is.finite(npar) && is.finite(npar_null) && npar <= npar_null) {
      warning("Full model does not have more parameters than the null model. LR test not performed.")
    } else {
      warning("Cannot perform LR test due to missing/invalid log-likelihoods or parameter counts.")
    }
  }
  object$LR.test <- c(LR = LR, df = d.f, `Pr(>Chi2)` = pvalLR) # Named vector, standard name for p-value col

  # --- 6. Create Coefficient Table ---
  zstat <- coef / se  # Will propagate NA if se is NA
  pval <- 2 * pnorm(-abs(zstat))
  coef_matrix <- cbind(Estimate = coef, `Std. Error` = se, `z value` = zstat, `Pr(>|z|)` = pval)

  # Store unrounded matrix for potential programmatic use
  object$coefficients_table_unrounded <- coef_matrix
  # Store rounded matrix for display (use print method for formatting)
  object$coefficients_table <- coef_matrix # Let print method handle rounding

  # --- 7. Calculate WTP ---
  # Assume wtp_spike function exists and works with the 'object' structure
  # Check if function exists before calling
  if (exists("wtp_spike", mode = "function")) {
    wtp_results <- tryCatch(
      wtp_spike(object = object), # Pass the *original* object structure? Check wtp_spike needs.
      error = function(e) {
        warning("wtp_spike function failed: ", e$message)
        NULL
      }
    )
    if (!is.null(wtp_results)) {
      object$medianWTP <- wtp_results$medianWTP
      object$meanWTP <- wtp_results$meanWTP
      object$trunc.meanWTP <- wtp_results$trunc.meanWTP
      object$adjtrunc.meanWTP <- wtp_results$adjtrunc.meanWTP
    } else {
      object$medianWTP <- NA; object$meanWTP <- NA; object$trunc.meanWTP <- NA; object$adjtrunc.meanWTP <- NA
    }
  } else {
    warning("wtp_spike function not found. Skipping WTP calculations.")
    object$medianWTP <- NA; object$meanWTP <- NA; object$trunc.meanWTP <- NA; object$adjtrunc.meanWTP <- NA
  }

  # --- 8. Finalise Output ---
  # Overwrite coefficients element with the table for standard summary output? Or keep separate?
  # Standard practice is often to replace object$coefficients with the table.
  object$coefficients <- coef_matrix # Replace scalar coefficients with the table
  class(object) <- "summary.spike"
  return(object)
}


# Example Usage (assuming Output_Baseline was created with dbspike_new):
# summary_results <- summary.spike(Output_Baseline)
# print(summary_results)


## Marginally faster
# summary.spike.new <- function(object, ...) {
#   # obtaining necessary components from the object
#   coef <- object$coefficients
#   npar <- length(coef)
#   dist <-  object$distribution
#
#
#   # Cache data evaluation
#   data_eval <- eval(object$data)
#
#
#   # Optimize SE calculation (Hessian inversion can be slow)
#   se <- sqrt(diag(solve(object$Hessian)))
#
#   # Common formula_null for all models
#   formula_null <- formula(object$formula)
#   formula_null[[3]][[2]] <- 1
#
#   # Estimate the null model based on class of the object
#   null_model <- switch(
#     class(object)[2],
#     "sbspike" = sbspike(
#       formula_null,
#       data = data_eval,
#       dist = dist,
#       par = coef[c(1, npar)]
#     ),
#     "dbspike" = dbspike(
#       formula_null,
#       data = data_eval,
#       dist = dist,
#       par = coef[c(1, npar)]
#     ),
#     "oohbspike" = oohbspike(
#       formula_null,
#       data = data_eval,
#       dist = dist,
#       par = coef[c(1, npar)]
#     ),
#     stop("object should be S3 class 'spike'")
#   )
#
#   # AIC and BIC calculation
#   object$AIC <- -2 * object$loglik + 2 * npar
#
#   # Likelihood Ratio Test
#   LR <- -2 * (null_model$loglik - object$loglik)
#   d.f <- length(coef) - length(null_model$coefficients)
#   pvalLR <- pchisq(LR, df = d.f, lower.tail = FALSE)
#   object$LR.test <- c(LR, d.f, pvalLR)
#
#   # Creating a table for coefficients, SE, etc.
#   zstat <- coef / se  # Z statistics
#   pval <- 2 * pnorm(-abs(zstat))  # P-values (no rounding yet)
#   coef_matrix <- cbind(coef, se, pval)
#   colnames(coef_matrix) <- c("Estimate", "Std. Error", "Pr(>|z|)")
#   object$coef <- round(coef_matrix, 3)  # Round at the end
#
#   # Computing WTP (check if `wtp_spike` can be optimized internally)
#   WTPs <- wtp_spike.new(object = object)
#   object$medianWTP <- WTPs$medianWTP
#   object$trunc.meanWTP <- WTPs$trunc.meanWTP
#
#   # Return the modified object
#   class(object) <- "summary.spike.new"
#   return(object)
# }


## Marginally faster
summary.spike <- function(object, ...) {
  # obtaining necessary components from the object
  coef <- object$coefficients
  npar <- length(coef)
  dist <-  object$distribution


  # Cache data evaluation
  data_eval <- eval(object$data)


  # Optimize SE calculation (Hessian inversion can be slow)
  se <- sqrt(diag(solve(object$Hessian)))

  # Common formula_null for all models
  formula_null <- formula(object$formula)
  formula_null[[3]][[2]] <- 1

  # Estimate the null model based on class of the object
  null_model <- switch(
    class(object)[2],
    "sbspike" = sbspike(
      formula_null,
      data = data_eval,
      dist = dist,
      par = coef[c(1, npar)]
    ),
    "dbspike" = dbspike(
      formula_null,
      data = data_eval,
      dist = dist,
      par = coef[c(1, npar)]
    ),
    "oohbspike" = oohbspike(
      formula_null,
      data = data_eval,
      dist = dist,
      par = coef[c(1, npar)]
    ),
    stop("object should be S3 class 'spike'")
  )

  # AIC and BIC calculation
  object$AIC <- c(
    AIC = -2 * object$loglik + 2 * npar,
    BIC = -2 * object$loglik + log(object$nobs) * npar
  )

  # Likelihood Ratio Test
  LR <- -2 * (null_model$loglik - object$loglik)
  d.f <- length(coef) - length(null_model$coefficients)
  pvalLR <- pchisq(LR, df = d.f, lower.tail = FALSE)
  object$LR.test <- c(LR, d.f, pvalLR)

  # Creating a table for coefficients, SE, etc.
  zstat <- coef / se  # Z statistics
  pval <- 2 * pnorm(-abs(zstat))  # P-values (no rounding yet)
  coef_matrix <- cbind(coef, se, zstat, pval)
  colnames(coef_matrix) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
  object$coef <- round(coef_matrix, 3)  # Round at the end

  # Computing WTP (check if `wtp_spike` can be optimized internally)
  WTPs <- wtp_spike(object = object)
  object$medianWTP <- WTPs$medianWTP
  object$meanWTP <- WTPs$meanWTP
  object$trunc.meanWTP <- WTPs$trunc.meanWTP
  object$adjtrunc.meanWTP <- WTPs$adjtrunc.meanWTP

  # Return the modified object
  class(object) <- "summary.spike"
  return(object)
}


# *************************************************************
#### Section 2: summary.spike: function that creates info for summary table ####
# *************************************************************
# summary.spike.old <- function(object,...){
#   # obtaining necessary components from the object
#   coef <- object$coefficients
#   npar <- length(coef)
#   se <- sqrt(diag(solve(object$Hessian)))
#   bid <- object$bid
#   X <- object$covariates
#   dist <-  object$distribution
#
#   # estimating the null model
#   formula_null <- formula(object$formula)
#   formula_null[[3]][[2]] <- 1
#   if(inherits(object, "sbspike")){
#     null_model <-   sbspike(formula_null,
#                             data = eval(object$data),
#                             dist = dist,
#                             par = coef[c(1, npar)])
#   }
#   else if(inherits(object, "dbspike")) {
#     null_model <-   dbspike(formula_null,
#                             data = eval(object$data),
#                             dist = dist,
#                             par = coef[c(1, npar)])
#   }
#   else if(inherits(object, "oohbspike")) {
#     null_model <- oohbspike(formula_null,
#                             data = eval(object$data),
#                             dist = dist,
#                             par = coef[c(1, npar)])
#   }
#   else {
#     stop("object should be S3 class 'spike'")
#   }
#
#   # function for obtaining AIC and BIC
#   akaike <- function(loglik, npar, k ){
#     -2*loglik + k*npar
#   }
#   object$AIC <- akaike(object$loglik, npar, k = c(2, log(object$nobs)))
#   names(object$AIC) <- c("AIC", "BIC")
#
#   # conducting likelihood ratio test
#   LR <- -2*(null_model$loglik - object$loglik)
#   d.f <- length(object$coefficients) - length(null_model$coefficients)
#   pvalLR <- pchisq(LR, df = d.f, lower.tail = FALSE)
#   object$LR.test <- c(LR, d.f, pvalLR)
#
#   # creating a table for coefficients, se, etc.
#   zstat <- coef/se  # z statistics
#   pval <- round(2*pnorm(-abs(zstat)), 3)  # p-value
#   coef <- cbind(coef, se, zstat, pval)
#   colnames(coef) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
#   object$coef <- coef
#
#   # computing WTP
#   WTPs <- wtp_spike(object = object)
#   object$medianWTP <- WTPs$medianWTP
#   object$meanWTP <- WTPs$meanWTP
#   object$trunc.meanWTP <- WTPs$trunc.meanWTP
#   object$adjtrunc.meanWTP <- WTPs$adjtrunc.meanWTP
#
#   # return output
#   class(object) <- "summary.spike"
#   return(object)
# }


# *************************************************************
#### Section 3: print.dbspike: function that prints spike model if converged ####
# *************************************************************
print.dbspike <- function(object, digits = max(3, getOption("digits") - 1), ...){
  if(!x$convergence)  cat("Too bad, the optimization did not converge\n")
  cat("\nDistribution:", object$distribution, "\n", sep = " ")
  print.default(format(object$coef, digits = digits), print.gap = 1, quote = FALSE)
  invisible(object)
}


# *************************************************************
#### Section 4: print.summary.spike: function to display coefficient estimates ####
# *************************************************************
print.summary.spike.new <- function(object,digits = max(3, getOption("digits") - 1),...){
  # cat("\nCall:",deparse(object$call, width.cutoff = floor(getOption("width") * 0.85)),"", sep = "\n")
  # cat("Formula:",deparse(object$formula, width.cutoff = floor(getOption("width") * 0.85)),"", sep = "\n")
  #
  # if(!object$convergence) {
  #   cat("The optimization did not converge\n")
  # }
  cat("Coefficients:", "\n")
  printCoefmat(object$coef, digits = 4, dig.tst = 4)

  # cat("\nDistribution:", object$distribution, "", sep = " ")
  cat("\nNumber of Obs.:", formatC(object$nobs, digits = 0), "\n")
  cat("Log-likelihood:", formatC(object$loglik, format="f", digits = digits), "\n")
  cat("\nLR statistic:", formatC(object$LR.test[1],
                                 format="f",
                                 digits = 3),
      "on",
      formatC(object$LR.test[2],
              digits = 0),

      "DF, p-value:", formatC(object$LR.test[3], format="f", digits = 3), "\n")
  cat("AIC:", formatC(object$AIC[1], format="f", digits = digits))

  cat("\nWTP estimates:")
  cat("\n Median:", object$medianWTP, sep = " ")
  cat("\n Trunc. Mean:", object$trunc.meanWTP,  sep = " ")
}


# *************************************************************
#### Section 5: print.summary.spike: function to display coefficient estimates ####
# *************************************************************
print.summary.spike <- function(object,digits = max(3, getOption("digits") - 1),...){
  cat("\nCall:",deparse(object$call, width.cutoff = floor(getOption("width") * 0.85)),"", sep = "\n")
  cat("Formula:",deparse(object$formula, width.cutoff = floor(getOption("width") * 0.85)),"", sep = "\n")

  if(!object$convergence) {
    cat("The optimization did not converge\n")
  }
  cat("Coefficients:", "\n")
  printCoefmat(object$coef, digits = 4, dig.tst = 4)

  cat("\nDistribution:", object$distribution, "", sep = " ")
  cat("\nNumber of Obs.:", formatC(object$nobs, digits = 0), "\n")
  cat("Log-likelihood:", formatC(object$loglik, format="f", digits = digits), "\n")
  cat("\nLR statistic:", formatC(object$LR.test[1], format="f", digits = 3),"on", formatC(object$LR.test[2], digits = 0),
      "DF, p-value:", formatC(object$LR.test[3], format="f", digits = 3), "\n")
  cat("AIC:", formatC(object$AIC[1], format="f", digits = digits),
      ", BIC:", formatC(object$AIC[2], format="f", digits = digits), "\n")
  cat("\nIterations:", formatC(object$niter, digits = 0), "")
  cat("\nConvergence:", object$convergence, "\n")

  cat("\nWTP estimates:")
  cat("\n Median:", object$medianWTP, sep = " ")
  if(is.finite(object$meanWTP)){
    cat("\n Mean:", object$meanWTP, "", sep = " ")
  } else {
    cat("\n Mean:", object$meanWTP, "(because of |beta_Lbid| < 1)", sep = " ")
  }
  cat("\n Trunc. Mean:", object$trunc.meanWTP,  sep = " ")
  cat("\n Adj. Trunc. Mean:", object$adjtrunc.meanWTP,  sep = " ")
}


# *************************************************************
#### Section 6: spikeCoef: function to estimate spikeCoef ####
# *************************************************************
spikeCoef <- function(object){
  X <- object$covariates
  B <- object$coefficients
  dist <- object$distribution

  names(B) <- NULL
  ncoefs   <- length(B)
  mX       <- colMeans(X)
  XB       <- sum(mX * B[-ncoefs])
  VCOV     <- vcov(object)[-ncoefs, -ncoefs]

  if(dist == "logistic" | dist == "log-logistic"){
    dlogisXB <- dlogis(XB)
    spike  <- 1/(1 + exp(XB))
    spikese <- as.vector(sqrt(dlogisXB %*% t(mX) %*% VCOV %*% mX %*% dlogisXB))
  }
  else if(dist == "normal" | dist == "log-normal"){
    dnormXB <- dnorm(XB)
    spike  <- pnorm(-XB)
    spikese <- as.vector(sqrt(dnormXB %*% t(mX) %*% VCOV %*% mX %*% dnormXB))
  }
  else if(dist == "weibull"){
    dweibullXB <- dweibull(XB,shape=1)
    spike  <- pweibull(exp(-XB),shape=1)
    spikese <- as.vector(sqrt(dweibullXB %*% t(mX) %*% VCOV %*% mX %*% dweibullXB))
  }

  #creating a table for coefficients, se, etc.
  z <- spike/spikese
  p <- round(2 * pnorm(-abs(z)), 6)
  rtn <- cbind(spike, spikese, z, p)
  colnames(rtn) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
  rownames(rtn) <- "Spike"
  rtn
}


## Just trunc mean and median
wtp_spike.new <- function(object, b = NULL) {
  # Determine if input is a model or just coefficients and covariates
  if (inherits(object, "dbspike")) {
    X <- object$covariates
    B <- object$coefficients
    Bid <- object$bid
    dist <- object$distribution
  } else {
    X <- object
    B <- b
  }

  names(B) <- NULL
  ncoefs <- length(B)
  Bbid <- B[ncoefs]  # Bid coefficient
  XB <- sum(colMeans(X) * B[-ncoefs])  # Compute XB once
  upper.bound <- max(Bid)

  # Function to calculate plogis or pnorm only once for truncated WTP
  get_adjusted_trunc_mean <- function(func, XB, Bbid, upper.bound, dist_fn) {
    trunc_mean <- integrate(func, 0, exp(upper.bound), stop.on.error = FALSE)$value
    return(list(trunc.meanWTP = trunc_mean))
  }

  # Determine distribution type and apply specific WTP computations
  if (dist == "logistic") {
    # Median WTP calculation
    medianWTP <- ifelse((1 / (1 + exp(-XB))) < 0.5, 0, (-XB / Bbid))

    if (Bbid > 0) {
      meanWTP <- NA
    } else {
      func <- function(x) plogis(-(XB + Bbid * x), lower.tail = FALSE)
      meanWTP <- integrate(func, 0, Inf, stop.on.error = FALSE)$value
      trunc_result <- get_adjusted_trunc_mean(func, XB, Bbid, upper.bound, plogis)
    }
  } else if (dist == "log-logistic") {
    medianWTP <- ifelse((1 / (1 + exp(-XB))) < 0.5, 0, exp(-XB / Bbid))

    if (Bbid > 0) {
      meanWTP <- NA
    } else {
      func <- function(x) plogis(-(XB + Bbid * log(x)), lower.tail = FALSE)
      meanWTP <- ifelse(abs(Bbid) > 1, integrate(func, 0, Inf, stop.on.error = FALSE)$value, Inf)
      trunc_result <- get_adjusted_trunc_mean(func, XB, Bbid, upper.bound, plogis)
    }
  } else if (dist == "normal") {
    medianWTP <- ifelse(pnorm(XB) < 0.5, 0, (-XB / Bbid))

    if (Bbid > 0) {
      meanWTP <- NA
    } else {
      func <- function(x) pnorm(-(XB + Bbid * x), lower.tail = FALSE)
      meanWTP <- integrate(func, 0, Inf, stop.on.error = FALSE)$value
      trunc_result <- get_adjusted_trunc_mean(func, XB, Bbid, upper.bound, pnorm)
    }
  } else if (dist == "log-normal") {
    medianWTP <- ifelse((1 / (1 + exp(-XB))) < 0.5, 0, exp(-XB / Bbid))

    if (Bbid > 0) {
      meanWTP <- NA
    } else {
      func <- function(x) pnorm(-(XB + Bbid * log(x)), lower.tail = FALSE)
      meanWTP <- integrate(func, 0, Inf, stop.on.error = FALSE)$value
      trunc_result <- get_adjusted_trunc_mean(func, XB, Bbid, upper.bound, pnorm)
    }
  } else if (dist == "weibull") {
    medianWTP <- ifelse(exp(exp(-XB)) < 0.5,
                        0,
                        (exp(XB)^(1 / abs(Bbid))) * log(2)^(1 / abs(Bbid)))

    if (Bbid > 0) {
      meanWTP <- NA
    } else {
      func <- function(x) pweibull(exp(-XB - Bbid * log(x)), shape = 1, lower.tail = FALSE)
      meanWTP <- ifelse(abs(Bbid) > 1, integrate(func, 0, Inf, stop.on.error = FALSE)$value, Inf)
      trunc_result <- get_adjusted_trunc_mean(func, XB, Bbid, upper.bound, function(x) pweibull(x, shape = 1))

    }
  }

  list(
    trunc.meanWTP = round(trunc_result$trunc.meanWTP, 3),
    medianWTP = round(medianWTP, 3)
  )
}


## THis is a marginally faster version than commented out version
wtp_spike <- function(object, b = NULL) {
  # Determine if input is a model or just coefficients and covariates
  if (inherits(object, "dbspike")) {
    X <- object$covariates
    B <- object$coefficients
    Bid <- object$bid
    dist <- object$distribution
  } else {
    X <- object
    B <- b
  }

  names(B) <- NULL
  ncoefs <- length(B)
  Bbid <- B[ncoefs]  # Bid coefficient
  XB <- sum(colMeans(X) * B[-ncoefs])  # Compute XB once
  upper.bound <- max(Bid)

  # Function to calculate plogis or pnorm only once for truncated WTP
  get_adjusted_trunc_mean <- function(func, XB, Bbid, upper.bound, dist_fn) {
    trunc_mean <- integrate(func, 0, exp(upper.bound), stop.on.error = FALSE)$value
    adjust_factor <- dist_fn(-(XB + Bbid * upper.bound))
    adjtrunc.meanWTP <- trunc_mean / adjust_factor
    return(list(trunc.meanWTP = trunc_mean, adjtrunc.meanWTP = adjtrunc.meanWTP))
  }

  # Determine distribution type and apply specific WTP computations
  if (dist == "logistic") {
    # Median WTP calculation
    medianWTP <- ifelse((1 / (1 + exp(-XB))) < 0.5, 0, (-XB / Bbid))

    if (Bbid > 0) {
      meanWTP <- NA
    } else {
      func <- function(x) plogis(-(XB + Bbid * x), lower.tail = FALSE)
      meanWTP <- integrate(func, 0, Inf, stop.on.error = FALSE)$value
      trunc_result <- get_adjusted_trunc_mean(func, XB, Bbid, upper.bound, plogis)
    }
  } else if (dist == "log-logistic") {
    medianWTP <- ifelse((1 / (1 + exp(-XB))) < 0.5, 0, exp(-XB / Bbid))

    if (Bbid > 0) {
      meanWTP <- NA
    } else {
      func <- function(x) plogis(-(XB + Bbid * log(x)), lower.tail = FALSE)
      meanWTP <- ifelse(abs(Bbid) > 1, integrate(func, 0, Inf, stop.on.error = FALSE)$value, Inf)
      trunc_result <- get_adjusted_trunc_mean(func, XB, Bbid, upper.bound, plogis)
    }
  } else if (dist == "normal") {
    medianWTP <- ifelse(pnorm(XB) < 0.5, 0, (-XB / Bbid))

    if (Bbid > 0) {
      meanWTP <- NA
    } else {
      func <- function(x) pnorm(-(XB + Bbid * x), lower.tail = FALSE)
      meanWTP <- integrate(func, 0, Inf, stop.on.error = FALSE)$value
      trunc_result <- get_adjusted_trunc_mean(func, XB, Bbid, upper.bound, pnorm)
    }
  } else if (dist == "log-normal") {
    medianWTP <- ifelse((1 / (1 + exp(-XB))) < 0.5, 0, exp(-XB / Bbid))

    if (Bbid > 0) {
      meanWTP <- NA
    } else {
      func <- function(x) pnorm(-(XB + Bbid * log(x)), lower.tail = FALSE)
      meanWTP <- integrate(func, 0, Inf, stop.on.error = FALSE)$value
      trunc_result <- get_adjusted_trunc_mean(func, XB, Bbid, upper.bound, pnorm)
    }
  } else if (dist == "weibull") {
    medianWTP <- ifelse(exp(exp(-XB)) < 0.5,
                        0,
                        (exp(XB)^(1 / abs(Bbid))) * log(2)^(1 / abs(Bbid)))

    if (Bbid > 0) {
      meanWTP <- NA
    } else {
      func <- function(x) pweibull(exp(-XB - Bbid * log(x)), shape = 1, lower.tail = FALSE)
      meanWTP <- ifelse(abs(Bbid) > 1, integrate(func, 0, Inf, stop.on.error = FALSE)$value, Inf)
      trunc_result <- get_adjusted_trunc_mean(func, XB, Bbid, upper.bound, function(x) pweibull(x, shape = 1))

    }
  }

  list(
    meanWTP = round(meanWTP, 3),
    trunc.meanWTP = round(trunc_result$trunc.meanWTP, 3),
    adjtrunc.meanWTP = round(trunc_result$adjtrunc.meanWTP, 3),
    medianWTP = round(medianWTP, 3)
  )
}


# *************************************************************
#### Section 7: #wtp_spike: function to calculate WTP at sample means ####
# *************************************************************
# wtp_spike.old <- function(object,b = NULL){
#   if(inherits(object, "dbspike")) {
#     X <- object$covariates
#     B <- object$coefficients
#     Bid <- object$bid
#     dist <- object$distribution
#   }
#   else {
#
#     X <- object
#     B <- b
#
#     # X <- object$covariates ## specifying "covariates" argument
#     # B <- object$coefficients ## changed from "b" to "B"
#     # Bid <- object$bid
#     # dist <- object$distribution
#   }
#
#   names(B) <- NULL
#   ncoefs   <- length(B)
#   Bbid     <- B[ncoefs]
#   XB <- sum(colMeans(X) * B[-ncoefs])
#   upper.bound <- max(Bid)
#
#   if(dist == "logistic"){
#     #median
#     if((1/(1 + exp(-XB))) < 0.5) {
#       medianWTP <- 0
#     }
#     else {
#       medianWTP <- (-XB/Bbid)
#     }
#     #mean
#     if (Bbid > 0) {
#       meanWTP <- NA
#     }
#     else {
#       func <- function(x) plogis(-(XB + Bbid * x), lower.tail = FALSE)
#       meanWTP <- integrate(func, 0, Inf, stop.on.error = FALSE)$value
#       trunc.meanWTP <- integrate(func, 0, upper.bound, stop.on.error = FALSE)$value
#       adjtrunc.meanWTP <- integrate(func, 0, upper.bound, stop.on.error = FALSE)$value / plogis(-(XB + Bbid * upper.bound))
#     }
#   }
#   else if(dist == "log-logistic"){
#     #median
#     if((1/(1 + exp(-XB))) < 0.5) {
#       medianWTP <- 0
#     }
#     else {
#       medianWTP <- exp(-XB/Bbid)
#     }
#     #mean
#     if (Bbid > 0) {
#       meanWTP <- NA
#     }
#     else {
#       func <- function(x) plogis(-(XB + Bbid * log(x)), lower.tail = FALSE)
#       meanWTP <- ifelse(abs(Bbid)>1,integrate(func, 0, Inf, stop.on.error = FALSE)$value,Inf)
#       trunc.meanWTP <- integrate(func, 0, exp(upper.bound), stop.on.error = FALSE)$value
#       adjtrunc.meanWTP <- integrate(func, 0, upper.bound, stop.on.error = FALSE)$value / pnorm(-(XB + Bbid * upper.bound))
#     }
#   }
#   else if(dist == "normal"){
#     #median
#     if(pnorm(XB) < 0.5) {
#       medianWTP <-0
#     }
#     else {
#       medianWTP <- (-XB/Bbid)
#     }
#     #mean
#     if (Bbid > 0) {
#       meanWTP <- NA
#     }
#     else {
#       func <- function(x) pnorm(-(XB + Bbid * x), lower.tail = FALSE)
#       meanWTP <- integrate(func, 0, Inf, stop.on.error = FALSE)$value
#       trunc.meanWTP <- integrate(func, 0, upper.bound, stop.on.error = FALSE)$value
#       adjtrunc.meanWTP <- integrate(func, 0, exp(upper.bound), stop.on.error = FALSE)$value / pnorm(-(XB + Bbid * upper.bound))
#     }
#   }
#   else if(dist == "log-normal"){
#     #median
#     if((1/(1 + exp(-XB))) < 0.5) {
#       medianWTP <- 0
#     }
#     else {
#       medianWTP <- exp(-XB/Bbid)
#     }
#     #mean
#     if (Bbid > 0) {
#       meanWTP <- NA
#     }
#     else {
#       func <- function(x) pnorm(-(XB + Bbid * log(x)), lower.tail = FALSE)
#       meanWTP <- integrate(func, 0, Inf, stop.on.error = FALSE)$value
#       trunc.meanWTP <- integrate(func, 0, exp(upper.bound), stop.on.error = FALSE)$value
#       adjtrunc.meanWTP <- integrate(func, 0, exp(upper.bound), stop.on.error = FALSE)$value / pnorm(-(XB + Bbid * upper.bound))
#       }
#   }
#   else if(dist == "weibull"){
#     #median
#     if(exp(exp(-XB)) < 0.5) {
#       medianWTP <- 0
#     }
#     else {
#       medianWTP <-(exp(XB)^(1/abs(Bbid)))*log(2)^(1/abs(Bbid))
#       #medianWTP <- exp(-XB/Bbid) * (log(2))^(-1/Bbid)
#     }
#     #mean
#     if (Bbid > 0) {
#       meanWTP <- NA
#     }
#     else{
#       func <- function(x) pweibull(exp(-XB - Bbid * log(x)), shape = 1, lower.tail = FALSE)
#       meanWTP <- ifelse(abs(Bbid)>1,integrate(func, 0, Inf, stop.on.error = FALSE)$value,Inf)
#       #meanWTP <- ifelse(abs(Bbid) > 1, exp(-XB/Bbid) * gamma(1 - 1/Bbid), Inf)
#       trunc.meanWTP <- integrate(func, 0, exp(upper.bound), stop.on.error = FALSE)$value
#       adjtrunc.meanWTP <- integrate(func, 0, exp(upper.bound), stop.on.error = FALSE)$value / pweibull(exp(-XB - Bbid * upper.bound), shape = 1)
#       }
#   }
#   list(
#     meanWTP = round(meanWTP, 3),
#     trunc.meanWTP = round(trunc.meanWTP, 3),
#     adjtrunc.meanWTP = round(adjtrunc.meanWTP, 3),
#     medianWTP = round(medianWTP, 3)
#   )
# }


# *************************************************************
#### Section 8: plot.spike: function to plot spike models ####
# *************************************************************
plot.spike <- function(object,type = "l",sub = NULL,xlab = "$ per month",ylab = "Probability of YES",lwd = 3,lty = 1,xlim = c(0, max(object$bid)),ylim = c(0, 1),bid = c(0, 1.2*max(object$bid)),...){
  #graphs for different distributions
  minbid <- min(bid)
  maxbid <- max(bid)
  bidseq <- seq(minbid, maxbid, by = (maxbid - minbid)/1000)
  b      <- object$coefficients
  dist   <- object$distribution
  npar   <- length(b)
  Xcov   <- colMeans(object$covariates)
  Vcov   <- sum(Xcov * b[-npar])
  V      <- Vcov + bidseq * b[npar]

  if (dist == "logistic") {
    pr <- plogis(-V, lower.tail = FALSE, log.p = FALSE)
    plot(x = c(0, bidseq),
         y = c(1, pr),
         xlab = xlab,
         ylab = ylab,
         main = "Logistic spike",
         lwd = lwd,
         type = type,
         xlim = xlim,
         ylim = ylim,
         axes = FALSE,
         ...)
    axis(side = 1, pos = 0)
    axis(side = 2, pos = 0)

    invisible(list(x = bidseq, utility = V, probability = pr))
  }
  else if (dist == "log-logistic") {
    pr <- plogis(exp(-V), lower.tail = FALSE, log.p = FALSE)
    plot(x = c(0, exp(bidseq)),
         y = c(1, pr),
         xlab = xlab,
         ylab = ylab,
         main = "Log-logistic spike",
         lwd = lwd,
         type = type,
         xlim = c(exp(minbid),exp(maxbid)),
         ylim = ylim,
         axes = FALSE,
         ...)
    axis(side = 1, pos = 0)
    axis(side = 2, pos = 0)

    invisible(list(x = bidseq, utility = V, probability = pr))
  }
  else if (dist == "normal") {
    pr <- pnorm(-V, lower.tail = FALSE, log.p = FALSE)
    plot(x = c(0, bidseq),
         y = c(1, pr),
         xlab = xlab,
         ylab = ylab,
         main = "Normal spike",
         lwd = lwd,
         type = type,
         xlim = xlim,
         ylim = ylim,
         axes = FALSE,
         ...)
    axis(side = 1, pos = 0)
    axis(side = 2, pos = 0)

    invisible(list(x = bidseq, utility = V, probability = pr))
  }
  else if (dist == "log-normal"){
    pr <- pnorm(-V, lower.tail = FALSE, log.p = FALSE)
    plot(x = c(0, exp(bidseq)),
         y = c(1, pr),
         xlab = xlab,
         ylab = ylab,
         main = "Log-normal spike",
         lwd = lwd,
         type = type,
         xlim = c(exp(minbid),exp(maxbid)),
         ylim = ylim,
         axes = FALSE,
         ...)
    axis(side = 1, pos = 0)
    axis(side = 2, pos = 0)

    invisible(list(x = bidseq, utility = V, probability = pr))
  }
  else if (dist == "weibull"){
    pr <- pweibull(exp(-V), shape=1,lower.tail = FALSE, log.p = FALSE)
    plot(x = c(0,exp(bidseq)),
         y = c(1,pr),
         xlab = xlab,
         ylab = ylab,
         main = "Weibull spike",
         lwd = lwd,
         type = type,
         xlim = c(exp(minbid),exp(maxbid)),
         ylim = ylim,
         axes = FALSE,
         ...)
    axis(side = 1, pos = 0)
    axis(side = 2, pos = 0)
  }

}


dbchoice <- function (formula, data, subset, na.action = na.omit, dist = "log-logistic",
                      par = NULL, ...)
{
  if (!inherits(formula, "Formula"))
    formula <- Formula(formula)
  if (!inherits(formula, "formula"))
    stop("invalid formula")
  if (length(formula[[2]]) != 3)
    stop("LHS variable in the formula must be like y1 + y2 ")

  # Simplified distribution validation with lookup
  valid_dists <- c("logistic", "log-logistic", "normal", "log-normal", "weibull")
  if (!(dist %in% valid_dists)) {
    stop("'dist' is incorrect.")
  }

  cl <- match.call()
  if (missing(data))
    stop("the name of the data frame object must be supplied in the 'data' argument")
  mf <- match.call(expand.dots = TRUE)
  m <- match(c("formula", "data", "subset", "na.action"),
             names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$formula <- formula
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  original.data <- data
  data <- mf

  # Extract response variables
  y1 <- model.part(formula, data = data, lhs = 1)[[1]]
  y2 <- model.part(formula, data = data, lhs = 1)[[2]]
  nobs <- length(y1)

  # Pre-compute response groups with one check for factor type
  is_factor_y1 <- is.factor(y1)
  if (is_factor_y1) {
    yy <- y1 == "yes" & y2 == "yes"
    yn <- y1 == "yes" & y2 == "no"
    ny <- y1 == "no" & y2 == "yes"
    nn <- y1 == "no" & y2 == "no"
  } else {
    yy <- y1 == 1 & y2 == 1
    yn <- y1 == 1 & y2 == 0
    ny <- y1 == 0 & y2 == 1
    nn <- y1 == 0 & y2 == 0
  }

  # Pre-compute indices once for repeated use
  yy_idx <- which(yy)
  yn_idx <- which(yn)
  ny_idx <- which(ny)
  nn_idx <- which(nn)

  # Create response index list for likelihood functions
  indices <- list(
    yy_idx = yy_idx,
    yn_idx = yn_idx,
    ny_idx = ny_idx,
    nn_idx = nn_idx
  )

  # Convert logical vectors to numeric only when needed for yvar
  yvar <- cbind(as.numeric(yy), as.numeric(yn), as.numeric(ny), as.numeric(nn))

  bid <- model.part(formula, data, lhs = 0, rhs = 2)
  BID <- pmin(bid[, 1], bid[, 2])  # More efficient than ifelse

  X <- model.part(formula, data, lhs = 0, rhs = 1)
  mmX <- model.matrix(formula, data, lhs = 0, rhs = 1)
  if (!any(colnames(mmX) == "(Intercept)")) {
    stop(message = "constant (intercept) term is required for the formula")
  }

  tmp.data <- data.frame(y1, mmX, BID)

  # Initial parameter estimation
  if (is.null(par)) {
    f.stage <- glm(y1 ~ . - 1, family = binomial(link = "probit"),
                   data = tmp.data)
    ini <- f.stage$coefficients
    npar <- length(ini)
    ini[npar] <- ifelse(ini[npar] > 0, -ini[npar], ini[npar])
    if (substr(dist, 1, 4) == "log-" | dist == "weibull")
      names(ini)[npar] <- "log(bid)"
    names(ini)[1] <- "(Intercept)"
  } else {
    if (length(par) != ncol(tmp.data) - 1)
      stop("the length of 'par' does not coincide with the number of explanatory variables.")
    ini <- par
    f.stage <- ini
  }

  # Optimised likelihood functions with pre-computed indices
  dbLL_logistic <- function(param, dvar, ivar, bid) {
    # Pre-compute matrix products once
    X1 <- cbind(ivar, bid[, 1])
    X2 <- cbind(ivar, bid[, 2])

    # Calculate all matrix products at once
    X1_param <- as.vector(X1 %*% param)
    X2_param <- as.vector(X2 %*% param)

    # Use pre-computed indices for subsetting
    ll_yy <- sum(plogis(-X2_param[yy_idx], lower.tail = FALSE, log.p = TRUE))
    ll_nn <- sum(plogis(-X2_param[nn_idx], lower.tail = TRUE, log.p = TRUE))

    # For yn cases - compute once and reuse
    if (length(yn_idx) > 0) {
      X2_yn_prob <- plogis(-X2_param[yn_idx], lower.tail = TRUE)
      X1_yn_prob <- plogis(-X1_param[yn_idx], lower.tail = TRUE)
      diff_yn <- X2_yn_prob - X1_yn_prob
      ll_yn <- sum(log(diff_yn))
    } else {
      ll_yn <- 0
    }

    # For ny cases - compute once and reuse
    if (length(ny_idx) > 0) {
      X1_ny_prob <- plogis(-X1_param[ny_idx], lower.tail = TRUE)
      X2_ny_prob <- plogis(-X2_param[ny_idx], lower.tail = TRUE)
      diff_ny <- X1_ny_prob - X2_ny_prob
      ll_ny <- sum(log(diff_ny))
    } else {
      ll_ny <- 0
    }

    ll <- ll_yy + ll_nn + ll_yn + ll_ny
    ifelse(is.finite(ll), -ll, NaN)
  }

  dbLL_normal <- function(param, dvar, ivar, bid) {
    # Pre-compute matrix products once
    X1 <- cbind(ivar, bid[, 1])
    X2 <- cbind(ivar, bid[, 2])

    # Calculate all matrix products at once
    X1_param <- as.vector(X1 %*% param)
    X2_param <- as.vector(X2 %*% param)

    # Use pre-computed indices for subsetting
    ll_yy <- sum(pnorm(-X2_param[yy_idx], lower.tail = FALSE, log.p = TRUE))
    ll_nn <- sum(pnorm(-X2_param[nn_idx], lower.tail = TRUE, log.p = TRUE))

    # For yn cases - compute once and reuse
    if (length(yn_idx) > 0) {
      X2_yn_prob <- pnorm(-X2_param[yn_idx], lower.tail = TRUE)
      X1_yn_prob <- pnorm(-X1_param[yn_idx], lower.tail = TRUE)
      diff_yn <- X2_yn_prob - X1_yn_prob
      ll_yn <- sum(log(diff_yn))
    } else {
      ll_yn <- 0
    }

    # For ny cases - compute once and reuse
    if (length(ny_idx) > 0) {
      X1_ny_prob <- pnorm(-X1_param[ny_idx], lower.tail = TRUE)
      X2_ny_prob <- pnorm(-X2_param[ny_idx], lower.tail = TRUE)
      diff_ny <- X1_ny_prob - X2_ny_prob
      ll_ny <- sum(log(diff_ny))
    } else {
      ll_ny <- 0
    }

    ll <- ll_yy + ll_nn + ll_yn + ll_ny
    ifelse(is.finite(ll), -ll, NaN)
  }

  dbLL_weibull <- function(param, dvar, ivar, bid) {
    # Pre-compute matrix products once
    X1 <- cbind(ivar, bid[, 1])
    X2 <- cbind(ivar, bid[, 2])

    # Calculate all matrix products at once and convert to vectors
    X1_param <- as.vector(X1 %*% param)
    X2_param <- as.vector(X2 %*% param)

    # Pre-compute exponentials
    exp_X1_param <- exp(-X1_param)
    exp_X2_param <- exp(-X2_param)

    # Use pre-computed indices for subsetting
    ll_yy <- sum(pweibull(exp_X2_param[yy_idx], shape = 1, lower.tail = FALSE, log.p = TRUE))
    ll_nn <- sum(pweibull(exp_X2_param[nn_idx], shape = 1, lower.tail = TRUE, log.p = TRUE))

    # For yn cases - compute once and reuse
    if (length(yn_idx) > 0) {
      X2_yn_prob <- pweibull(exp_X2_param[yn_idx], shape = 1, lower.tail = TRUE)
      X1_yn_prob <- pweibull(exp_X1_param[yn_idx], shape = 1, lower.tail = TRUE)
      diff_yn <- X2_yn_prob - X1_yn_prob
      ll_yn <- sum(log(diff_yn))
    } else {
      ll_yn <- 0
    }

    # For ny cases - compute once and reuse
    if (length(ny_idx) > 0) {
      X1_ny_prob <- pweibull(exp_X1_param[ny_idx], shape = 1, lower.tail = TRUE)
      X2_ny_prob <- pweibull(exp_X2_param[ny_idx], shape = 1, lower.tail = TRUE)
      diff_ny <- X1_ny_prob - X2_ny_prob
      ll_ny <- sum(log(diff_ny))
    } else {
      ll_ny <- 0
    }

    ll <- ll_yy + ll_nn + ll_yn + ll_ny
    ifelse(is.finite(ll), -ll, NaN)
  }

  # Use function dispatch table instead of if-else cascade
  dbLL_functions <- list(
    "logistic" = dbLL_logistic,
    "log-logistic" = dbLL_logistic,
    "normal" = dbLL_normal,
    "log-normal" = dbLL_normal,
    "weibull" = dbLL_weibull
  )

  # Select appropriate likelihood function
  dbLL <- dbLL_functions[[dist]]

  # Optimisation with more efficient control parameters
  suppressWarnings(dbchoice <- optim(ini, fn = dbLL, method = "BFGS",
                                     hessian = TRUE, dvar = yvar, ivar = mmX, bid = bid,
                                     control = list(abstol = 1e-30, maxit = 1000, reltol = 1e-12)))

  npar <- length(dbchoice$par)
  terms <- terms(formula)

  # Process factor variables more efficiently
  fac <- which(sapply(X, is.factor))
  xlevels <- vector("list", length(fac))
  names(xlevels) <- names(fac)

  for (i in seq_along(fac)) {
    xlevels[[i]] <- levels(X[[fac[i]]])
  }

  contrasts <- attr(mmX, "contrasts")
  output <- list(f.stage = f.stage, dbchoice = dbchoice, coefficients = dbchoice$par,
                 call = cl, formula = formula, Hessian = dbchoice$hessian,
                 distribution = dist, loglik = -dbchoice$value, convergence = dbchoice$convergence == 0,
                 niter = dbchoice$counts, nobs = nobs,
                 covariates = mmX, bid = bid, yn = cbind(y1, y2), data.name = data,
                 terms = terms, contrasts = contrasts, data = original.data,
                 xlevels = xlevels)
  class(output) <- "dbchoice"
  return(output)
}


# *************************************************************
#### Section 9: plot.dbchoice: function to plot DBDC models ####
# *************************************************************
plot.dbchoice <- function(object,type = "l",sub = NULL,xlab = "€ per month",ylab = "Probability of YES",lwd = 3,lty = 1,xlim = c(0, max(object$bid)),ylim = c(0, 1),bid = c(0, max(object$bid)),...){
  #graphs for different distributions
  minbid <- min(bid)
  maxbid <- max(bid)
  bidseq <- seq(minbid, maxbid, by = (maxbid - minbid)/1000)
  b      <- object$coefficients
  dist   <- object$distribution
  npar   <- length(b)
  Xcov   <- colMeans(object$covariates)
  Vcov   <- sum(Xcov * b[-npar])
  V      <- Vcov + bidseq * b[npar]

  if (dist == "logistic") {
    pr <- plogis(-V, lower.tail = FALSE, log.p = FALSE)
    plot(x = c(0, bidseq),
         y = c(1, pr),
         xlab = xlab,
         ylab = ylab,
         main = "Logistic DBDC model",
         lwd = lwd,
         type = type,
         xlim = xlim,
         ylim = ylim,
         axes = FALSE,
         ...)
    axis(side = 1, pos = 0)
    axis(side = 2, pos = 0)

    invisible(list(x = bidseq, utility = V, probability = pr))
  }
  else if (dist == "log-logistic") {
    pr <- plogis(-V, lower.tail = FALSE, log.p = FALSE)
    plot(x = c(0, exp(bidseq)),
         y = c(1, pr),
         xlab = xlab,
         ylab = ylab,
         main = "Log-logistic DBDC model",
         lwd = lwd,
         type = type,
         xlim = c(exp(minbid),exp(maxbid)),
         ylim = ylim,
         axes = FALSE,
         ...)
    axis(side = 1, pos = 0)
    axis(side = 2, pos = 0)

    invisible(list(x = bidseq, utility = V, probability = pr))
  }
  else if (dist == "normal") {
    pr <- pnorm(-V, lower.tail = FALSE, log.p = FALSE)
    plot(x = c(0, bidseq),
         y = c(1, pr),
         xlab = xlab,
         ylab = ylab,
         main = "Normal DBDC model",
         lwd = lwd,
         type = type,
         xlim = xlim,
         ylim = ylim,
         axes = FALSE,
         ...)
    axis(side = 1, pos = 0)
    axis(side = 2, pos = 0)

    invisible(list(x = bidseq, utility = V, probability = pr))
  }
  else if (dist == "log-normal"){
    pr <- pnorm(-V, lower.tail = FALSE, log.p = FALSE)
    plot(x = c(0, exp(bidseq)),
         y = c(1, pr),
         xlab = xlab,
         ylab = ylab,
         main = "Log-normal DBDC model",
         lwd = lwd,
         type = type,
         xlim = c(exp(minbid),exp(maxbid)),
         ylim = ylim,
         axes = FALSE,
         ...)
    axis(side = 1, pos = 0)
    axis(side = 2, pos = 0)

    invisible(list(x = bidseq, utility = V, probability = pr))
  }
  else if (dist == "weibull"){
    pr <- pweibull(exp(-V), shape=1,lower.tail = FALSE, log.p = FALSE)
    plot(x = c(0, exp(bidseq)),
         y = c(1, pr),
         xlab = xlab,
         ylab = ylab,
         main = "Weibull DBDC model",
         lwd = lwd,
         type = type,
         xlim = c(exp(minbid),exp(maxbid)),
         ylim = ylim,
         axes = FALSE,
         ...)
    axis(side = 1, pos = 0)
    axis(side = 2, pos = 0)
  }

}


wtp_i_New <- function(object){
  X <- object$covariates
  B <- object$coefficients
  dist <- object$distribution
  Bid <- object$bid

  names(B) <- NULL
  ncoefs   <- length(B)
  Bbid     <- B[ncoefs]
  XB <- cbind(rowSums(X %*% B[-ncoefs]))

  n = length(XB)
  iwtp.tmean <- matrix(NA, nrow = n, ncol = 1)
  iwtp.median <- matrix(NA, nrow = n, ncol = 1)
  upper.bound <- 2 * max(Bid)

  for (i in 1:n){
    if(dist == "logistic"){
      func <- function(z) plogis(-(XB[i] + Bbid * z), lower.tail = FALSE)
      iwtp.tmean[i] <- integrate(func, 0,upper.bound , stop.on.error = FALSE)$value
      iwtp.median[i] <- (-XB[i]/Bbid)
    }
    else if(dist == "log-logistic"){
      func <- function(z) plogis(-(XB[i] + Bbid * log(z)), lower.tail = FALSE)
      iwtp.tmean[i] <- integrate(func, 0, exp(upper.bound), stop.on.error = FALSE)$value
      iwtp.median[i] <- exp(-XB[i]/Bbid)
    }
    else if(dist == "weibull"){
      func <- function(z) pweibull(exp(-XB[i] - Bbid * log(z)), shape = 1, lower.tail = FALSE)
      iwtp.tmean[i] <- integrate(func, 0, exp(upper.bound), stop.on.error = FALSE)$value
      iwtp.median[i] <- (exp(XB[i])^(1/abs(Bbid)))*log(2)^(1/abs(Bbid))
    }
    else if(dist == "normal"){
      func <- function(z) pnorm(-(XB[i] + Bbid * z), lower.tail = FALSE)
      iwtp.tmean[i] <- integrate(func, 0, upper.bound, stop.on.error = FALSE)$value
      iwtp.median[i] <- (-XB[i]/Bbid)
    }
    else if(dist == "log-normal"){
      func <- function(z) pnorm(-(XB[i] + Bbid * log(z)), lower.tail = FALSE)
      iwtp.tmean[i] <- integrate(func, 0, exp(upper.bound), stop.on.error = FALSE)$value
      iwtp.median[i] <- exp(-XB[i]/Bbid)
    }
  }
  pdf_iwtp <-
    cbind(iwtp.tmean, iwtp.median)
  colnames(pdf_iwtp) <-
    c("individual truncated mean WTP",
      "individual median WTP")
  pdf_iwtp
}


# *************************************************************
#### Section 10: wtp_i: function to obtain individual-level WTPs ####
# *************************************************************
# wtp_i <- function(object){
#   X <- object$covariates
#   B <- object$coefficients
#   dist <- object$distribution
#   Bid <- object$bid
#
#   names(B) <- NULL
#   ncoefs   <- length(B)
#   Bbid     <- B[ncoefs]
#   XB <- cbind(rowSums(X%*%B[-ncoefs]))
#
#   n=length(XB)
#   iwtp.mean <- matrix(NA,nrow=n,ncol=1)
#   iwtp.tmean <- matrix(NA,nrow=n,ncol=1)
#   iwtp.median <- matrix(NA,nrow=n,ncol=1)
#   iwtp.adjtruncmean <- matrix(NA,nrow=n,ncol=1)
#   upper.bound <- 2*max(Bid)
#
#   for (i in 1:n){
#     if(dist == "logistic"){
#       func <- function(z) plogis(-(XB[i] + Bbid * z), lower.tail = FALSE)
#       iwtp.mean[i] <- integrate(func, 0, Inf, stop.on.error = FALSE)$value
#       iwtp.tmean[i] <- integrate(func, 0,upper.bound , stop.on.error = FALSE)$value
#       iwtp.adjtruncmean[i] <- integrate(func, 0, max(object$bid), stop.on.error = FALSE)$value / plogis(-(XB[i] + Bbid  * max(object$bid)))
#       iwtp.median[i] <- (-XB[i]/Bbid)
#     }
#     else if(dist == "log-logistic"){
#       func <- function(z) plogis(-(XB[i] + Bbid * log(z)), lower.tail = FALSE)
#       iwtp.mean[i] <- ifelse(abs(Bbid)>1,integrate(func, 0, Inf, stop.on.error = FALSE)$value,Inf)
#       iwtp.tmean[i] <- integrate(func, 0, exp(upper.bound), stop.on.error = FALSE)$value
#       iwtp.adjtruncmean[i] <- integrate(func, 0, exp(max(object$bid)), stop.on.error = FALSE)$value / plogis(-(XB[i] + Bbid  * max(object$bid)))
#       iwtp.median[i] <- exp(-XB[i]/Bbid)
#     }
#     else if(dist == "weibull"){
#       func <- function(z) pweibull(exp(-XB[i] - Bbid * log(z)), shape = 1, lower.tail = FALSE)
#       iwtp.mean[i] <- ifelse(abs(Bbid)>1, (exp(XB[i])^(1/abs(Bbid)))*gamma(1/abs(Bbid)+1),Inf)
#       iwtp.tmean[i] <- integrate(func, 0, exp(upper.bound), stop.on.error = FALSE)$value
#       iwtp.adjtruncmean[i] <- integrate(func, 0, exp(max(object$bid)), stop.on.error = FALSE)$value / pweibull(exp(-XB[i] - Bbid * max(object$bid)), shape = 1)
#       iwtp.median[i] <- (exp(XB[i])^(1/abs(Bbid)))*log(2)^(1/abs(Bbid))
#     }
#     else if(dist == "normal"){
#       func <- function(z) pnorm(-(XB[i] + Bbid * z), lower.tail = FALSE)
#       iwtp.mean[i] <- integrate(func, 0, Inf, stop.on.error = FALSE)$value
#       iwtp.tmean[i] <- integrate(func, 0, upper.bound, stop.on.error = FALSE)$value
#       iwtp.adjtruncmean[i] <- integrate(func, 0, max(object$bid), stop.on.error = FALSE)$value / pnorm(-(XB[i] + Bbid  * max(object$bid)))
#       iwtp.median[i] <- (-XB[i]/Bbid)
#     }
#     else if(dist == "log-normal"){
#       func <- function(z) pnorm(-(XB[i] + Bbid * log(z)), lower.tail = FALSE)
#       iwtp.mean[i] <- integrate(func, 0, Inf, stop.on.error = FALSE)$value
#       iwtp.tmean[i] <- integrate(func, 0, exp(upper.bound), stop.on.error = FALSE)$value
#       iwtp.adjtruncmean[i] <- integrate(func, 0, exp(max(object$bid)), stop.on.error = FALSE)$value / pnorm(-(XB[i] + Bbid  * max(object$bid)))
#       iwtp.median[i] <- exp(-XB[i]/Bbid)
#     }
#   }
#   pdf_iwtp <-
#     cbind(iwtp.mean, iwtp.tmean, iwtp.adjtruncmean, iwtp.median)
#   colnames(pdf_iwtp) <-
#     c("individual mean WTP",
#       "individual truncated mean WTP",
#       "individual mean WTP (truncated at the maximum bid with adjustment)",
#       "individual median WTP")
#   pdf_iwtp
# }


## THIS VERSION FULLY REWRITTEN WITH AI ASSISTANCE FOR SPEED IMPROVEMENTS

wtp_i <- function(object) {
  # Extract once to avoid repeated lookups
  X <- object$covariates
  B <- object$coefficients
  dist <- object$distribution
  Bid <- object$bid
  max_bid <- max(Bid)
  upper_bound <- 2 * max_bid

  # Vectorised computation
  ncoefs <- length(B)
  Bbid <- B[ncoefs]
  XB <- as.vector(X %*% B[-ncoefs])  # Faster than cbind(rowSums())

  n <- length(XB)
  results <- matrix(NA, nrow = n, ncol = 4)

  # Pre-compute distribution-specific functions outside the loop
  if (dist == "logistic") {
    compute_mean <- function(i) {
      func <- function(z) plogis(-(XB[i] + Bbid * z), lower.tail = FALSE)
      integrate(func, 0, Inf, stop.on.error = FALSE)$value
    }
    compute_tmean <- function(i) {
      func <- function(z) plogis(-(XB[i] + Bbid * z), lower.tail = FALSE)
      integrate(func, 0, upper_bound, stop.on.error = FALSE)$value
    }
    compute_adjtruncmean <- function(i) {
      func <- function(z) plogis(-(XB[i] + Bbid * z), lower.tail = FALSE)
      integrate(func, 0, max_bid, stop.on.error = FALSE)$value / plogis(-(XB[i] + Bbid * max_bid))
    }
    compute_median <- function(i) -XB[i]/Bbid
  } else if (dist == "log-logistic") {
    compute_mean <- function(i) {
      if (abs(Bbid) <= 1) return(Inf)
      func <- function(z) plogis(-(XB[i] + Bbid * log(z)), lower.tail = FALSE)
      integrate(func, 0, Inf, stop.on.error = FALSE)$value
    }
    compute_tmean <- function(i) {
      func <- function(z) plogis(-(XB[i] + Bbid * log(z)), lower.tail = FALSE)
      integrate(func, 0, exp(upper_bound), stop.on.error = FALSE)$value
    }
    compute_adjtruncmean <- function(i) {
      func <- function(z) plogis(-(XB[i] + Bbid * log(z)), lower.tail = FALSE)
      integrate(func, 0, exp(max_bid), stop.on.error = FALSE)$value / plogis(-(XB[i] + Bbid * max_bid))
    }
    compute_median <- function(i) exp(-XB[i]/Bbid)
  } else if (dist == "weibull") {
    compute_mean <- function(i) {
      if (abs(Bbid) <= 1) return(Inf)
      (exp(XB[i])^(1/abs(Bbid)))*gamma(1/abs(Bbid)+1)
    }
    compute_tmean <- function(i) {
      func <- function(z) pweibull(exp(-XB[i] - Bbid * log(z)), shape = 1, lower.tail = FALSE)
      integrate(func, 0, exp(upper_bound), stop.on.error = FALSE)$value
    }
    compute_adjtruncmean <- function(i) {
      func <- function(z) pweibull(exp(-XB[i] - Bbid * log(z)), shape = 1, lower.tail = FALSE)
      integrate(func, 0, exp(max_bid), stop.on.error = FALSE)$value / pweibull(exp(-XB[i] - Bbid * max_bid), shape = 1)
    }
    compute_median <- function(i) (exp(XB[i])^(1/abs(Bbid)))*log(2)^(1/abs(Bbid))
  } else if (dist == "normal") {
    compute_mean <- function(i) {
      func <- function(z) pnorm(-(XB[i] + Bbid * z), lower.tail = FALSE)
      integrate(func, 0, Inf, stop.on.error = FALSE)$value
    }
    compute_tmean <- function(i) {
      func <- function(z) pnorm(-(XB[i] + Bbid * z), lower.tail = FALSE)
      integrate(func, 0, upper_bound, stop.on.error = FALSE)$value
    }
    compute_adjtruncmean <- function(i) {
      func <- function(z) pnorm(-(XB[i] + Bbid * z), lower.tail = FALSE)
      integrate(func, 0, max_bid, stop.on.error = FALSE)$value / pnorm(-(XB[i] + Bbid * max_bid))
    }
    compute_median <- function(i) -XB[i]/Bbid
  } else if (dist == "log-normal") {
    compute_mean <- function(i) {
      func <- function(z) pnorm(-(XB[i] + Bbid * log(z)), lower.tail = FALSE)
      integrate(func, 0, Inf, stop.on.error = FALSE)$value
    }
    compute_tmean <- function(i) {
      func <- function(z) pnorm(-(XB[i] + Bbid * log(z)), lower.tail = FALSE)
      integrate(func, 0, exp(upper_bound), stop.on.error = FALSE)$value
    }
    compute_adjtruncmean <- function(i) {
      func <- function(z) pnorm(-(XB[i] + Bbid * log(z)), lower.tail = FALSE)
      integrate(func, 0, exp(max_bid), stop.on.error = FALSE)$value / pnorm(-(XB[i] + Bbid * max_bid))
    }
    compute_median <- function(i) exp(-XB[i]/Bbid)
  }

  # Parallelizable loop (could use foreach if dataset is large)
  for (i in 1:n) {
    results[i, 1] <- compute_mean(i)
    results[i, 2] <- compute_tmean(i)
    results[i, 3] <- compute_adjtruncmean(i)
    results[i, 4] <- compute_median(i)
  }

  colnames(results) <- c("individual mean WTP",
                         "individual truncated mean WTP",
                         "individual mean WTP (truncated at the maximum bid with adjustment)",
                         "individual median WTP")
  results
}


plot_wtp_distributions <- function(wtp_data, xlim = NULL) {
  # Load required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is needed for this function")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("tidyr package is needed for this function")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package is needed for this function")
  }
  library(ggplot2)
  library(tidyr)
  library(dplyr)

  # Convert matrix to data frame
  if (is.matrix(wtp_data)) {
    wtp_df <- as.data.frame(wtp_data)
  } else {
    wtp_df <- wtp_data
  }

  # Rename columns for clarity
  names(wtp_df) <- c("Mean", "TruncMean", "AdjTruncMean", "Median")

  # Create long format for density plotting
  wtp_long <- wtp_df %>%
    dplyr::select(Median, TruncMean) %>%
    pivot_longer(cols = c(Median, TruncMean),
                 names_to = "Measure",
                 values_to = "WTP")

  # Set default limits if not provided
  if (is.null(xlim)) {
    # Set reasonable x-limits based on data
    max_value <- min(max(wtp_df$TruncMean, na.rm = TRUE),
                     quantile(wtp_df$TruncMean, 0.99, na.rm = TRUE) * 1.5)
    xlim <- c(0, max_value)
  }

  # Calculate summary statistics
  summary_stats <- wtp_long %>%
    group_by(Measure) %>%
    summarise(
      Mean = mean(WTP, na.rm = TRUE),
      Median = median(WTP, na.rm = TRUE),
      Q1 = quantile(WTP, 0.25, na.rm = TRUE),
      Q3 = quantile(WTP, 0.75, na.rm = TRUE),
      Min = min(WTP, na.rm = TRUE),
      Max = max(WTP, na.rm = TRUE)
    )

  # Create plot with all requested modifications:
  # 1. Fixed color aesthetic in geom_density
  # 2. Removed line legend entries
  # 3. Repositioned legend to top-right
  # 4. Proper coord_cartesian syntax
  plot_list <- ggplot(wtp_long, aes(x = WTP, fill = Measure)) +

    geom_density(alpha = 0.5, color = "black") +

    geom_vline(
      data = summary_stats,
      aes(xintercept = Mean, group = Measure),
      linetype = "dashed",
      linewidth = 1,
      color = c("#E41A1C", "#377EB8") # Manually match Set1 colors
    ) +

    scale_fill_brewer(
      palette = "Set1",
      labels = c("Median" = "Individual Median WTP",
                 "TruncMean" = "Individual Truncated Mean WTP")
    ) +

    scale_x_continuous(breaks = seq.int(from = 0, to = 3000, by = 250)) +
    coord_cartesian(xlim = c(0, 3000)) +

    labs(
      title = "Distribution of Individual WTP Estimates",
      subtitle = paste0(
        "Mean of Medians: ",
        round(summary_stats$Mean[summary_stats$Measure == "Median"], 1),
        ", Mean of Truncated Means: ",
        round(summary_stats$Mean[summary_stats$Measure == "TruncMean"], 1)
      ),
      x = "WTP (USD)",
      y = "Density"
    ) +

    theme_bw() +

    theme(
      legend.title = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.8, 0.9),
      legend.background = element_rect(fill = "white", color = NA, linewidth = 0.5)
    )

  return(plot_list)
}


# Example usage with your data:
# plot_wtp_distributions(Model_Income_Base_WTP, xlim = c(0, 3500))
# Example usage:
# Assuming wtp_results is the matrix or data frame from wtp_i() function:
# plots <- plot_wtp_distributions(wtp_results)
#
# # View density plot
# plots$density
#
# # View boxplot
# plots$boxplot
#
# # View scatter plot
# plots$scatter
#
# # View ratio histogram
# plots$ratio
#
# # View QQ plot
# plots$qqplot
#
# # To save plots:
# ggsave("wtp_density.png", plots$density, width = 8, height = 6)


# *************************************************************
#### Section 11: Krinsky Robb for Weibull spike ####
# *************************************************************
krCI.wbspike <- function(
    object,
    nsim = 1000,
    CI = 0.95,
    individual = NULL)

{
  # checking arguments
  if(CI > 1) {
    stop("CI must be between 0 and 1")
  }
  if(!inherits(object, "spike")) {
    stop("the object must be spike class")
  }

  # setting objects
  X       <- object$covariates             # the covariates
  npar    <- length(object$coefficients)   # the number of coefficients
  coef    <- object$coefficients           # coefficient estimates
  formula <- formula(object$formula, lhs = 0, rhs = -2)
  VCOV    <- solve(object$Hessian)

  # simulating coefficients
  kr.coef    <- t(mvrnorm(nsim, coef, VCOV))   # mvrnorm in MASS package
  kr.coefbid <- kr.coef[npar,  , drop = FALSE] # coefficient on the bid
  kr.coefcov <- kr.coef[-npar, , drop = FALSE] # coefficients on the covariates

  if(is.null(individual)) {
    kr.Xb <- colSums(colMeans(X[, -npar, drop = FALSE]) * kr.coefcov)
  } else {
    mf.nexX <- model.frame(formula, individual, xlev = object$xlevels)
    mm.newX <- model.matrix(formula, mf.nexX, contrasts.arg = object$contrasts)
    newX    <- as.vector(mm.newX)
    kr.Xb   <- colSums(newX * kr.coefcov)
  }

  # calculating and storing mean and median WTPs
  kr.median  <- ifelse(exp(exp(-kr.Xb)) < 0.5, 0,(exp(kr.Xb)^(1/abs(kr.coefbid)))*log(2)^(1/abs(kr.coefbid)))
  mbid <- exp(max(object$bid))
  func.kr <- function(x, A, B) pweibull(exp(-A - B*log(x)), shape=1, lower.tail=FALSE)
  denom <- function(A, B) pweibull(exp(-A - B*log(mbid)), shape=1)
  kr.meanWTP <- numeric(nsim)
  kr.trunc.meanWTP <- numeric(nsim)

  for(i in 1:nsim){
    kr.meanWTP[i] <- integrate(func.kr, 0, Inf, A = kr.Xb[i], B = kr.coefbid[i], stop.on.error = FALSE)$value
    kr.trunc.meanWTP[i] <- integrate(func.kr, 0, mbid, A = kr.Xb[i], B = kr.coefbid[i], stop.on.error = FALSE)$value
  }

  output <- list(mWTP = kr.trunc.meanWTP, medWTP = kr.median)

  # sorting the simulation outcomes
  kr.trunc.meanWTP <- sort(kr.trunc.meanWTP, na.last = TRUE)
  kr.median  <- sort(kr.median)

  lb <- 0.5*(1 - CI)   # lower bound of the empirical distribution
  ub <- CI + lb        # upper bound of the empirical distribution
  int <- c(ceiling(nsim*lb), floor(nsim*ub))

  # 100*CI% simulated confidence intervals
  CImat <- rbind(kr.trunc.meanWTP[int], kr.median[int])

  # the mean estimates in the original outcome
  if (is.null(individual)) {
    tmp.sum <- wtp_spike(object = object)
  } else {
    tmp.sum <- wtp_spike(object = mm.newX, b = object$coefficients,Bid = object$bid)
  }

  out <- cbind(c(tmp.sum$trunc.meanWTP, tmp.sum$medianWTP), CImat)
  rownames(out) <- c("Trunc. Mean", "Median")
  colnames(out) <- c("Estimate", "LB", "UB")

  output$out <- out

  class(output) <- "krCI"
  return(output)
}


Hypertension_Estimators_Loaded <- TRUE
