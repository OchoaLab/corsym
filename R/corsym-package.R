#' CorSym: Correlation Estimation For Exchangeable/Symmetrical Variables
#'
#' This package implements [corsym()], an unbiased estimator of the correlation for exchangeable data, where the values (x,y) in a pair have no inherent order, so replacing with (y,x) randomly in any or all pairs is equally valid.
#' It also provides functions to modify variable orders with [partial_order()] and a wrapper around the standard [pearson()] estimator to illustrate its problematic dependence on data ordering (under the hypothesis that order is actually meaningless).
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
#'
#' # with the data in random order, Pearson and CorSym agree roughly
#' pearson( X )
#' corsym( X )
#'
#' # but Pearson gives a very different answer for extremely ordered data
#' # while CorSym gives exactly the same answer both ways
#' pearson( Xe )
#' corsym( Xe )
#' 
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
