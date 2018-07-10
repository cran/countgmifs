poisson.theta<-function (par, w, x, y, offset, beta) {
  if (!is.null(offset)) {
	  Xb <- cbind(offset, w, x) %*% c(1, par, beta)
	}  
  else {
   	Xb <- cbind(w, x) %*% c(par, beta)
  }
  contri.LL<-y*Xb-exp(Xb)-lgamma(y+1) # likelihood function Poisson
  loglik <- sum(contri.LL)
  -loglik
}