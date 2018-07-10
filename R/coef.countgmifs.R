#' Extract Model Coefficients.
#'
#' A generic function which extracts the model coefficients from a fitted model object fit using \code{countgmifs}
#' @param object an \code{countgmifs} fitted object.
#' @param model.select when \code{x} is specified any model along the solution path can be selected. The default is \code{model.select="BIC"} which calculates the predicted values using the coefficients from the model having the lowest BIC. Other options are \code{model.select="AIC"} or any numeric value from the solution path.
#' @param dots other arguments.
#' @keywords methods
#' @export
#' @examples
#' coef.countgmifs()

coef.countgmifs <- function(object, model.select="BIC", ...) {
    if (model.select=="AIC") {
    	model.select = which.min(object$AIC[-1])
    } else if (model.select=="BIC") {
    	model.select = which.min(object$BIC[-1])
    }
	if (is.null(object$x)) {
		if (dim(object$w)[2]!=0) {
			if (object$family=="nb") {
				coef<-c(object$a[model.select], object$theta[model.select,])
				names(coef)<- c("alpha", colnames(object$w))
			} else {
				coef<-c(object$theta[model.select,])
				names(coef)<- c(colnames(object$w))
			}
		} 
	} else if (dim(object$w)[2]!=0) {
		if (object$family=="nb") {
			coef<-c(object$a[model.select], object$theta[model.select,], object$beta[model.select,])
			names(coef)<- c("alpha",colnames(object$w), colnames(object$x))	
		} else {
			coef<-c(object$theta[model.select,], object$beta[model.select,])
			names(coef)<- c(colnames(object$w), colnames(object$x))
		}
	} else {
		if (object$family=="nb") {
			coef<-c(object$a[model.select], object$beta[model.select,])
			names(coef)<- c("alpha", colnames(object$x))
		} else {
			coef<-c(object$beta[model.select,])
			names(coef)<- c(colnames(object$x))
		}
	}	
	coef
}
	










