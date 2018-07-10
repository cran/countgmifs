nb.theta<-function (par, a, w, x, y, offset, beta) {
	b<- par
	if (!is.null(offset)) {
		Xb <- cbind(offset, w, x) %*% c(1, b, beta)
	} else {
		Xb <- cbind(w, x) %*% c(b, beta)
	}
	contri.LL<- y*log((a*exp(Xb))/(1+ (a*exp(Xb)))) -(1/a)*log(1+ (a*exp(Xb))) + lgamma(y+ (1/a)) - lgamma(y+1) - lgamma(1/a)
	# likelihood fxn
	loglik <- sum(contri.LL)
	-loglik
}