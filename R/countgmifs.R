#' Discrete Response Generalized Monotone Incremental Forward Stagewise Regression.
#'
#' This function can fit a Poisson or negative binomial model when the number of parameters exceeds the sample size, using the the generalized monotone incremental forward stagewise method.
#' @param formula an object of class "\code{formula}" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The left side of the formula is the ordinal outcome while the variables on the right side of the formula are the covariates that are not included in the penalization process. Note that if all variables in the model are to be penalized, an intercept only model formula should be specified.
#' @param data an optional data frame, list or environment (or object coercible by \code{as.data.frame} to a data frame) containing the variables in the model.
#' @param x an optional matrix of predictors that are to be penalized in the model fitting process.
#' @param offset
#' @param subset an optional vector specifying a subset of observations to be used in the fitting process.
#' @param epsilon small incremental amount used to update a coefficient at a given step.
#' @param tol the iterative process stops when the difference between successive log-likelihoods is less than this specified level of tolerance.
#' @param scale logical, if TRUE (default) the penalized predictors are centered and scaled.
#' @param verbose logical, if TRUE the step number is printed to the console (default is FALSE).
#' @param family the type of count response model to be fit. Default is 'nb' for negative binomial; user can also specify 'poisson'.
#' @param dots other arguments.
#' @keywords methods
#' @keywords regression
#' @export
#' @examples
#' countgmifs()

countgmifs<-function (formula, data, x=NULL, offset, subset, epsilon=0.001, tol=1e-5, scale=TRUE, verbose=FALSE,
  family = "nb", ...) {
	mf <- match.call(expand.dots = FALSE)
	cl <- match.call()
	m <- match(c("formula", "data", "subset", "offset"), names(mf), 0L)
	mf <- mf[c(1L, m)]
	mf[[1L]] <- as.name("model.frame")
	mf <- eval(mf, parent.frame())
	mt <- attr(mf, "terms")
	y <- model.response(mf)
	w <- model.matrix(mt, mf)
	offset <- model.offset(mf)
	#### Subset code
	if (!is.null(x)) {
		if (missing(subset))
			r <- TRUE
		else {
			e <- substitute(subset)
			r <- eval( e, data)
			if (!is.logical(r))
				stop("'subset' must evaluate to logical" )
			r <- r & !is.na(r)
		}
	if (sum(class(x)%in%"character")==1) {
		nl <- as.list( 1:ncol(data))
		names(nl) <- names( data)
		vars <- eval(substitute(x), nl, parent.frame())
		x <- data [r , vars, drop=FALSE ]
		x <- as.matrix(x )
	} else if ( sum(class(x)%in%"matrix")==1 || sum(class(x)%in%"data.frame")==1) {
		x <- x[r,, drop =FALSE]
		x <- as.matrix(x)
	}
	}
	#### End subset code
	if (is.na(match(family, c("poisson", "nb")))) {
        stop("Error:")
        cat("Only poisson and nb are available for family.\n")
    }
	if(!is.null(offset)){
		offset<- log(offset)
	}
	data <- data.matrix(data)
	n<- length(y)
	if (!is.null(x)) {
		vars <- dim(x)[2]
		# vars is the number of penalized variables
		oldx <- x
		if (scale) {
			x <- scale(x, center = TRUE, scale = TRUE)
			# Center and scale the penalized variables
		}
		x_original<-x
		# Keep the old x, will use in Hilbe's and estimation of theta
		x <- cbind(x, -1 * x)
		# x is now the expanded x matrix
		beta <- rep(0, dim(x)[2])
		# Beta as a vector of 0's with a length equivalent the the expanded x
		names(beta) <- dimnames(x)[[2]]
		step <- 1
		Estimates <- matrix(0,ncol=vars)
		# Estimates will be the final collapsed beta values- matrix
		if (family=="nb") {
			if(!is.null(offset)){
				initialize<-glm.nb(y~w-1 + offset(offset), control=glm.control(maxit=100))
				# Starting values theta and (Intercept)
			} else {
				initialize<-glm.nb(y~w-1,control=glm.control(maxit=100))
			}
			a<- 1/theta.mm(initialize)
			# Alpha for model with no penalized predictors,
			# use mm estimate to initialize
			a.update<- a
		} else {
			if(!is.null(offset)){
				initialize<-glm(y~w-1, offset=offset, family=poisson)
			} else {
				initialize<-glm(y~w-1, family=poisson)
			}
		}
		LL0 <- Likelihood <- logLik(initialize)
		# Log-likelihood for model with no penalized predictors
		AIC<-AIC(initialize)
		# AIC for model with no penalized predictors
		BIC<-BIC(initialize)
		# BIC for model with no penalized predictors
		theta <- coef(initialize)
		# Unpenalized coefficient estimates for model
		# with no penalized predictors
		theta.update <- matrix(theta, ncol = length(theta))
		# a.update will be used to keep track of all alpha estimates
		repeat {
			step <- 1 + step
			# Xb will be calculated depending on whether offset is present
			# and whether there are penalized variables
			if (!is.null(offset)) {
				Xb <- cbind(offset, w, x) %*% c(1, theta, beta)
			} else {
				Xb <- cbind(w, x) %*% c(theta, beta)
			}
			if (family=="nb") {
				u <- t(x) %*% ((y- exp(Xb)) /(1+ (a*exp(Xb))))
			} else {
				u <- t(x)%*%(y-exp(Xb))
			}
			# Likelihood gradient value- NEGATIVE BINOMIAL Hilbe Page 192
			update.j <- which.min(-u)
			# Choose coeffiecient to update
			if (-u[update.j] < 0) {
				beta[update.j] <- beta[update.j] + epsilon
				# Update beta
			}
			Estimates<-rbind(Estimates,beta[1:vars]-beta[(vars+1):length(beta)])
			# Keep track of beta changes
			# Update intercept and non-penalized subset using new beta values
			if (family=="nb") {
				out <- optim(theta, nb.theta, a=a, w=w, x=x_original, y=y, offset=offset, beta=beta[1:vars]-beta[(vars+1):length(beta)], method="BFGS")
				theta <- out$par
				# Update alpha using Hilbe algorithm. Need to use the original x not the expanded
				a<- hilbe(w=w,y=y,x=x_original,theta=theta, beta=beta[1:vars]-beta[(vars+1):length(beta)], offset= offset, delta=1e-5)
				a.update<- c(a.update,a)
				# Number of predictors in the NB model: nonzero beta + theta + alpha(1)
				p <- sum(Estimates[step,]!=0) + length(theta) + 1
				Likelihood[step]<- LL1<- -out$value
        		AIC[step]<- 2*p-2*Likelihood[step]
        		BIC[step]<- p*log(n) - 2*Likelihood[step]
			} else {
				out <- optim(theta, poisson.theta, w=w, x=x, y=y, offset=offset, beta=beta, method="BFGS")
				theta <- out$par
				p <- sum(Estimates[step+1,]!=0) + length(theta)
				Likelihood[step]<- LL1<- -out$value
        		AIC[step]<- 2*p-2*Likelihood[step]
        		BIC[step]<- p*log(n) - 2*Likelihood[step]
			}
			# Keep track of theta values
			theta.update <- rbind(theta.update, theta)
			#if (!is.null(offset)) {
			#	# Calculate Xb to be used to calculate the Likelihood
			#	Xb_LL <- cbind(offset, w, x_original) %*%c(1, theta, beta[1:vars]-beta[(vars+1):length(beta)])
			#} else {
			#	Xb_LL <- cbind(w, x_original) %*%c(theta,beta[1:vars]-beta[(vars+1):length(beta)])
			#}
			#Likelihood[step]<-LL1<- sum(y*log((a*exp(Xb_LL))/(1+ (a*exp(Xb_LL)))) - (1/a)*log(1+ (a*exp(Xb_LL))) + lgamma(y+ (1/a)) - lgamma(y+1) - lgamma(1/a))
			## likelihood function- NEGATIVE BINOMIAL Hilbe pg 190
			#AIC[step] <- 2*p - 2*Likelihood[step]
			## AIC - equation 5.16 Hilbe pg 68
			#BIC[step] <- p*log(n) - 2*Likelihood[step]
			## BIC - equation 5.21 Hilbe pg 71
			if (verbose) cat("step = ", step, "\n")
			# STOPPING CRITERIA
			if (step >= 1 && ( (abs(LL1- LL0)<tol) ||(p>=n-1) )) {
				break
			}
			LL0 <- LL1
			# Assign the "old" LL value the "new" LL value for the next step
			}
			if (family=="nb") {
				output<-list(a=a.update, beta = Estimates, theta=theta.update, x=oldx, y=y, scale=scale, logLik=Likelihood, AIC=AIC, BIC=BIC,w=w,offset=offset, family=family)
			} else {
				output<-list(beta = Estimates, theta=theta.update, x=oldx, y=y, scale=scale, logLik=Likelihood, AIC=AIC, BIC=BIC, w=w,offset=offset, family=family)
			}
	    class(output) <- "countgmifs"
		} else {
			if (family=="nb") {
				out<-glm.nb(y~w-1, offset=offset)
				output <- list(coef(out), a=1/out$theta)
			} else {
				output<-glm(y~w-1, offset=offset, family=poisson)
			}
		}
		#class(output) <- "countgmifs"
		output
}
