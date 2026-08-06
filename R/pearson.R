#' Calculate Pearson estimates with confidence intervals (CIs)
#'
#' This function is a simple wrapper around [stats::cor()] with the same interface and missingness handling of [corsym()], particularly it adds CIs to the estimates.
#' Pairs with any missingness are automatically ignored (matching [stats::cor()] with non-default option `use = 'complete.obs'`).
#' Also, unlike [stats::cor()], this function accepts only two variables as inputs and does not calculate correlation matrices, just the correlation between the two variables.
#' Pearson is included in this package to constrast to [corsym()] in handling exchangeable variables.
#'
#' @inheritParams add_cor_CI
#' @param x First vector of variables, or a 2-column matrix with the variables of interest
#' @param y Second vector of variables
#'
#' @return A vector with three values: the Pearson correlation estimate between `x` and `y`, and the lower and upper CIs.
#'
#' @examples
#' # generate some random independent data, just for example
#' n <- 53
#' x <- rnorm( n )
#' y <- rnorm( n )
#' # calculate Pearson correlation with CIs
#' pearson( x, y )
#' 
#' @export
pearson <- function( x, y = NULL, alpha = 0.05, ci_offset = 3 ) {
    # normalize inputs
    obj <- handle_args( x, y )
    x <- obj$x
    y <- obj$y
    n <- obj$n

    # calculate default correlation
    r <- stats::cor( x, y )
    # use external function to add CIs
    # flatten to vector
    return( as.numeric( add_cor_CI( r, n, alpha = alpha, ci_offset = ci_offset ) ) )
}
