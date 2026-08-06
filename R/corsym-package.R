#' CorSym: Correlation Estimation For Exchangeable/Symmetrical Variables
#'
#' This package implements [corsym()], an unbiased estimator of the correlation for exchangeable data, where the values (x,y) in a pair have no inherent order, so replacing with (y,x) randomly in any or all pairs is equally valid.
#' It also provides functions to modify variable orders with [partial_order()] and [randomize_order()], and a wrapper around the standard [pearson()] estimator to illustrate its problematic dependence on data ordering (under the hypothesis that order is actually meaningless).
#' [order_bias_test()] provides a statistical test for order bias.
#' Lastly, [add_cor_CI()] calculates confidence intervals (CIs) to existing correlation estimates with sample sizes (used internally by both [corsym()] and [pearson()]).
#'
#' @examples
#' # We simulate simple independent (uncorrelated) data for these examples.
#' # (Correlated data can be simulated with the `mvtnorm` and other packages,
#' # we do this in our paper, but here it is omitted to reduce package dependencies.)
#' # number of pairs
#' n <- 47
#' # matrix with different pairs in rows 
#' X <- matrix( rnorm( n * 2 ), nrow = n, ncol = 2 )
#'
#' # create a version with extreme order (every pair starts with the minimum value,
#' # ends with the maximum in the pair):
#' Xe <- partial_order( X )
#' # and you can randomize biased orders too:
#' Xr <- randomize_order( Xe )
#'
#' # with the data in random order, Pearson and CorSym agree roughly
#' pearson( X )
#' corsym( X )
#'
#' # with a second random reordering, Pearson differs slightly,
#' # while Corsym gives exactly the same answer as before
#' pearson( Xr )
#' corsym( Xr )
#'
#' # but Pearson gives a very different answer for extremely ordered data
#' # while CorSym gives exactly the same answer both ways
#' pearson( Xe )
#' corsym( Xe )
#'
#' # test the data for order bias
#' # count the number of times the first value was the smaller one of the pair,
#' # separately for the original data, the second randomization,
#' # and the extreme order data
#' data <- data.frame(
#'   type = c('Random 1', 'Random 2', 'Extreme'),
#'   x = c(
#'         sum( X[,1] < X[,2] ),
#'         sum( Xr[,1] < Xr[,2] ),
#'         sum( Xe[,1] < Xe[,2] )
#'   ),
#'   n = n
#' )
#' # this function reports the frequency of those cases,
#' # and p-values under the null hypothesis that the true frequency is 0.5
#' order_bias_test( data )
#' 
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
