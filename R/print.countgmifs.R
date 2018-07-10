#' Print the Contents of a Count GMIFS Fitted Object.
#'
#' This function prints the names of the list objects from an \code{countgmifs} fitted model
#' @param object an \code{countgmifs} fitted object.
#' @param dots other arguments.
#' @keywords methods
#' @export
#' @examples
#' print.countgmifs()

print.countgmifs <-
function(x, ...) {
	print(names(x))
}