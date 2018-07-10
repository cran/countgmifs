fitted.countgmifs<- function(object, neww = NULL, newdata, newx = NULL, model.select="BIC", ...) {
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
    family <- object$family
	offset<-object$offset
    if (!is.null(x)) {
        if (dim(w)[2] != 0) {
            if (is.null(dim(object$theta))) {
                theta <- object$theta[model.select]
            }
            else {
                theta <- object$theta[model.select,]
            }
        }
        beta <- object$beta[model.select,]
    } else {
        if (dim(w)[2] != 0) {
            theta <- object$theta ######## why no [model.select,] on theta here?
        }
    }
    if (family=="nb") {
    	alpha<-object$a[model.select]
    }
    if (!is.null(neww))
    	if (neww==~1) {
    		m <- model.frame(neww)
    	} else {
    	m <- model.frame(neww, newdata) 
        neww <- model.matrix(neww, m)
        neww <- neww[,-grep("(Intercept)",dimnames(neww)[[2]]),drop=FALSE]
        }
    if (!is.null(newx)) 
        newx <- as.matrix(newx)
    if (is.null(neww) & is.null(newx)) {
        neww <- object$w
        newx <- object$x
    }
    n <- max(dim(neww)[1], dim(newx)[1])
    if (!is.null(newx) & identical(newx, x)) {
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
    } else if (!is.null(newx) && object$scale) {
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
        newx <- matrix(newx[-(1:dim(x)[1]), ], ncol = dim(x)[2])
    }
    if (dim(w)[2] != 0) {
        if (is.null(x)) {
        	if (!is.null(offset)) {
				Xb <- cbind(offset, neww) %*% c(1, theta)
			} else {
				Xb <- neww %*% theta
			}
        } else if (!is.null(x)) {
			if (!is.null(offset)) {
				Xb <- cbind(offset, neww, newx) %*% c(1, theta, beta)
			} else {
				Xb <- cbind(neww, newx) %*% c(theta, beta)
			}            
        }
    } else if (!is.null(x)) {
		if (!is.null(offset)) {    
        	Xb <- cbind(offset, newx) %*% c(1, beta)
        } else {
        	Xb <- newx %*% beta
        }
    } else {
        Xb <- 0
    }
	if (is.null(offset)) {
      y.pred <- exp(c(theta,beta) %*% t(cbind(neww, newx)))
    } else {
      y.pred <- exp(c(1,theta,beta) %*% t(cbind(offset, neww, newx)))
    }
	y.pred
}