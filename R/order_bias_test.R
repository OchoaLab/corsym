#' Test for order bias
#'
#' This function performs a test for whether data is appearing in a biased order or not.
#' The test calculates a two-sided Binomial p-value under the null hypothesis that the true frequency of the counted order is `f0` (0.5 by default).
#'
#' @param data A data.frame (including tibble) with at least two columns: `n` the sample size (number of pairs), and `x` the number of pairs in a given order.
#' @param f0 The expected frequency of pairs in the given order, under the null hypothesis.
#'
#' @return The input data frame with two more columns: `f` is the sample order frequency (equal to `x/n`), and `pval` the two-sided Binomial p-value.
#'
#' @examples
#' # toy data used in the test
#' data <- data.frame(
#'   n = c( 30, 50, 100 ),
#'   x = c( 12, 20,  13 )
#' )
#'
#' # add frequencies and p-values!
#' data <- order_bias_test( data )
#' 
#' @export
order_bias_test <- function( data, f0 = 0.5 ) {
    # validate input
    if ( missing( data ) )
        stop( '`data` is missing!' )
    if ( !is.data.frame( data ) )
        stop( '`data` must be a data.frame (including tibble)!' )
    if ( ! 'x' %in% names( data ) )
        stop( '`data` must have column `x` (number of ordered pairs)!' )
    if ( ! 'n' %in% names( data ) )
        stop( '`data` must have column `n` (number of pairs)!' )
    
    # calculate order proportion, include in table
    data$f <- data$x / data$n
    
    # NOTE: x-1 for upper tail only, so p-val = Prob( X >= x ), not just > (default, which for first row returns useless p=0 otherwise).  Double minimum of both p-values to make it a two-sided p-value
    # in one case both tails give a value bigger than 1 after doubling (because both cases count itself and it's a discrete distribution), cap at 1 in that case
    data$pval <- 2 * pmin(
                         stats::pbinom( data$x, data$n, f0 ),
                         stats::pbinom( data$x - 1, data$n, f0, lower.tail = FALSE ),
                         0.5
                     )
    
    # return
    data
}
