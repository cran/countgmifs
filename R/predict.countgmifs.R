#' Predict Outcome for Count GMIFS Fitted Model.
#'
#' This function returns a numeric vector that is the predicted response from the \code{countgmifs} fitted object.
#' @param object an \code{ordinalgmifs} fitted object.
#' @param neww an optional formula that includes the unpenalized variables to use for predicting the response. If omitted, the training data are used.
#' @param newdata an optional data.frame that minimally includes the unpenalized variables to use for predicting the response. If omitted, the training data are used.
#' @param newx an optional matrix of penalized variables to use for predicting the response. If omitted, the training data are used.
#' @param model.select when \code{x} is specified any model along the solution path can be selected. The default is \code{model.select="BIC"} which calculates the predicted values using the coefficients from the model having the lowest BIC. Other options are \code{model.select="AIC"} or any numeric value from the solution path.
#' @param dots other arguments.
#' @keywords methods
#' @export
#' @examples
#' predict.countgmifs()

predict.countgmifs <- function(object, neww = NULL, newdata, newx = NULL, model.select="BIC", newoffset=NULL, ...) {
	#browser()
    if (model.select == "AIC") {
        model.select = which.min(object$AIC[-1])
    }
    else if (model.select == "BIC") {
        model.select = which.min(object$BIC[-1])
    }
    y <- object$y
    w <- object$w
    x <- object$x
    if (!is.null(newx)) {
       if (class(newx)=="data.frame") newx<-as.matrix(newx)
	identical.x<-all.equal(object$x, newx, check.attributes=FALSE) 
    }
    family <- object$family
    offset<-object$offset
    if (!is.null(newx) && !is.null(offset) && is.null(newoffset)) stop("newoffset must be specified because offset was specified in the model fit")
    new.w <- neww
    if (is.null(neww) || neww==~1) {
		if (!is.null(newx)) {
			neww <- as.matrix(w[1:dim(newx)[1],drop=FALSE])
		} else {
			neww <- w			
		}
   } else {
    	 m <- model.frame(neww, newdata) 
       neww <- model.matrix(neww, m)
    }
    if (!is.null(newx)) {
	  if (is.logical(identical.x)) {
          if (object$scale) {
            sd <- apply(newx, 2, sd)
            for (i in 1:dim(newx)[2]) {
                if (sd[i] == 0) {
                  newx[, i] <- scale(newx[, i], center = TRUE, 
                    scale = FALSE)
                }
                else {
                  newx[, i] <- scale(newx[, i], center = TRUE, 
                    scale = TRUE)
                }
            }
        } 
       } else if (object$scale) {
           newx <- rbind(x, newx)
           sd <- apply(newx, 2, sd)
           for (i in 1:dim(newx)[2]) {
            if (sd[i] == 0) {
                newx[, i] <- scale(newx[, i], center = TRUE, 
                  scale = FALSE)
            } else {
                newx[, i] <- scale(newx[, i], center = TRUE, 
                  scale = TRUE)
            }
        }
        newx <- newx[-(1:dim(x)[1]),,drop=FALSE]
      }
    }
#    if (dim(neww)[2]!=dim(w)[2]) stop("neww must include the same number of covariates as the original model")
#    if (!is.null(newx) && dim(newx)[2]!=dim(x)[2]) stop("newx must include the same number of covariates as the original model")	
#    # scale newx if scale is TRUE
	if (!is.null(newx)) {
		beta <- object$beta[model.select,]
		if ( dim(w)[2]==1 ) {
			theta <- object$theta[model.select]
		} else {
			theta <- object$theta[model.select,]
		}
		if (!is.null(newoffset)) {
			Xb <- cbind(newoffset, neww, newx) %*% c(1, theta, beta)
		} else {
			Xb <- cbind(neww, newx) %*% c(theta, beta) # This is the error, there is no neww
		}  
	}  else if (is.null(newx) & is.null(new.w)) {
		neww <- w
		newx <- x
		if (object$scale) {
              sd <- apply(newx, 2, sd)
              for (i in 1:dim(newx)[2]) {
                if (sd[i] == 0) {
                  newx[, i] <- scale(newx[, i], center = TRUE, 
                    scale = FALSE)
                }
                else {
                  newx[, i] <- scale(newx[, i], center = TRUE, 
                    scale = TRUE)
                }
               }
            } 
		beta <- object$beta[model.select,]
		if ( dim(w)[2]==1 ) {
			theta <- object$theta[model.select]
		} else {
			theta <- object$theta[model.select,]
		}
		if (!is.null(newoffset)) {
			Xb <- cbind(newoffset, neww, newx) %*% c(1, theta, beta)
		} else {
			Xb <- cbind(neww, newx) %*% c(theta, beta) 
		}  		
	} else if (is.null(newx) && !is.null(new.w)) {
		    if (!is.null(newoffset)) {
				Xb <- cbind(newoffset, neww) %*% c(1, theta)
			} else {
				Xb <- neww %*% theta
			}
	} 
     y.pred <- exp(Xb)
     as.numeric(y.pred)
}
