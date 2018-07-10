#' Plot Solution Path for a Count GMIFS Fitted Model.
#'
#' This function plots either the coefficient path, the AIC, or the log-likelihood for a fitted \code{countgmifs} object.
#' @param x a \code{countgmifs} object.
#' @param type default is \code{"trace"} which plots the coefficient path for the fitted object. Also available are \code{"AIC"}, \code{"BIC"}, and \code{"logLik"}.
#' @param xlab a default x-axis label will be used which can be changed by specifying a user-defined x-axis label.
#' @param ylab a default y-axis label will be used which can be changed by specifying a user-defined y-axis label.
#' @param main a default main title will be used which can be changed by specifying a user-defined main title.
#' @param dots other arguments.
#' @keywords methods
#' @export
#' @examples
#' plot.countgmifs()

plot.countgmifs <- function (x, type = "trace", xlab=NULL, ylab=NULL, main=NULL, ...) 
{
	if (is.null(xlab)) xlab="Step"
	if (is.null(ylab)) {
		if (type == "AIC") {
			ylab="AIC"
		} else if (type == "BIC") {
			ylab="BIC"
		} else if (type == "trace") {
			ylab=expression(hat(beta))
		} else if (type == "logLik") {
			ylab = "logLikelihood"
		}
	}
    if (is.null(x$x)) {
        stop("Penalized model not requested\n")
    }
    else if (type == "trace") {
        matplot(x$beta, ylab = ylab, xlab = xlab, 
            type = "l")
    }
    else if (type == "AIC") {
        plot(x$AIC, xlab = xlab, ylab = ylab)
    }
    else if (type == "BIC") {
        plot(x$BIC, xlab = xlab, ylab = ylab)
    }
    else if (type == "logLik") {
        plot(x$logLik, xlab = xlab, ylab = ylab)
    }
    if (is.null(main)) {
        	title(paste(x$family, "GMIFS", sep = " "))
    } else title(main)
}