hilbe<- function(w, y, x, theta, beta, offset, delta) {
	mu<- mean(y) # estimate lambda
	chi2<- sum( ((y-mu)^2)/mu) # Poisson chi2 test
	if(!is.null(x) & !is.null(w)) {
		df<-length(y)- dim(w)[2]-sum(beta!=0)
	} else if(is.null(x) & !is.null(w)) {
		df<-length(y)- dim(w)[2]
	} else if(!is.null(x) & is.null(w)) {
		df<-length(y)-sum(beta!=0)
	}
	disp<- chi2/df # Poisson Dispersion
	alpha<- 1/disp # Inverse of Poisson Dispersion
	j<-1
	delta_disp <- 1.0 # Initiating the change in the dispersion estimate
	while(abs(delta_disp) >= delta) {
		old_disp<- disp
		if (is.null(x)) {
			if (!is.null(offset)) {
				Xb <- cbind(offset, w) %*% c(1, theta)
			} else {
				Xb <- w %*% theta
			}
		} else {
			if (!is.null(offset)) {
				Xb <- cbind(offset, w, x) %*% c(1, theta, beta)
			} else {
				Xb <- cbind(w, x) %*% c(theta, beta)
			}
		}
		mu<- exp(Xb)
		chi2<- ((y-mu)^2) / (mu + (alpha*(mu^2))) # Negative Binomial Chi2 test
		chi2<- sum ( chi2)
		disp<- chi2/df
		alpha<- disp* alpha
		delta_disp<- disp- old_disp
		j=j+1
	}
	alpha
}