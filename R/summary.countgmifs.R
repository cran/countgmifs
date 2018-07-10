#' Summarize a Count GMIFS Object.
#'
#' Prints the following items extracted from the fitted \code{countgmifs} object: the family used and model parameter estimates. For models that include \code{x}, the parameter estimates, AIC, BIC, and log-likelihood are printed for indicated \code{model.select} step or if \code{model.select} is not supplied the step at which the minimum BIC was observed.
#' @param object an \code{countgmifs} fitted object.
#' @param model.select when \code{x} is specified any model along the solution path can be selected. The default is \code{model.select="BIC"} which calculates the predicted values using the coefficients from the model having the lowest BIC. Other options are \code{model.select="AIC"} or any numeric value from the solution path.
#' @param dots other arguments.
#' @keywords methods
#' @export
#' @examples
#' summary.countgmifs()

summary.countgmifs <-function (object, model.select = "BIC", ...)
{
	if (is.null(object$x)) {
		coef <- coef(object)
		coef
	} else {
        if (model.select == "AIC") {
            model.select = which.min(object$AIC[-1])
        }
        else if (model.select == "BIC") {
            model.select = which.min(object$BIC[-1])
        }
        coef <- coef(object, model.select)
        cat(object$family, "model \n")
        if (object$family == "nb" ) {
            cat("alpha      = ", object$a[model.select], "\n")
        }
        if (is.null(object$x)) {
            cat("logLik     = ", object$logLik, "\n")
        }
        if (!is.null(object$x)) {
            cat("at step    = ", model.select, "\n")
            cat("logLik     = ", object$logLik[model.select], "\n")
            cat("AIC        = ", object$AIC[model.select], "\n")
            cat("BIC        = ", object$BIC[model.select], "\n")
        }
        cat("\n")
        coef
    }
}
