#' Calculate confidence intervals (CIs) for correlation estimates
#'
#' This function calculates CIs based on the Fisher transformation and theory originally derived for Pearson estimator, now validated empirically for CorSym.
#' Only the mean estimates and the sample sizes are needed for these calculations.
#'
#' @param r Correlation estimates.
#' Can be scalar or vector, each non-NA value must be between -1 and 1, inclusive.
#' Cases with NA `r` have all `NA` outputs (means and CIs).
#' @param n Sample sizes.
#' Can be scalar or vector.
#' If both `r` and `n` are vectors, they must have the same length.
#' Cases with NAs or with `n < ci_offset` have NA CIs.
#' @param alpha The complement of the confidence level (default 0.05 results in 1 - 0.05 = 95% CIs).
#' @param ci_offset The amount by which the sample size gets reduced in the variance conversion formula.
#' For Normal data, Pearson should use 3 and CorSym 1.5, but different values may be better for other distributions.
#'
#' @return A matrix with a row for every input (single row if `r` and `n` are both scalars, as many rows as the lenghts of either `r` or `n` otherwise), and three named columns: `r` repeats the input, and `CIL` and `CIU` are the lower and upper CIs.
#'
#' @examples
#' # make up values for example
#' # a vector of pre-calculated correlation estimates
#' rs <- c( 0.3, 0.5, 0.9 )
#' # and sample sizes
#' ns <- c( 10, 100, 1000 )
#'
#' # calculate CIs for these three cases:
#' out <- add_cor_CI( rs, ns )
#'
#' @seealso
#' [corsym()] and [pearson()] use this function internally.
#' 
#' @export
add_cor_CI <- function( r, n, alpha = 0.05, ci_offset = 3 ) {
    # some validations
    if ( missing( r ) )
        stop( '`r` is required!' )
    if ( missing( n ) )
        stop( '`n` is required!' )
    if ( any( r < -1, na.rm = TRUE ) )
        stop( '`r` must be >= -1!' )
    if ( any( r > 1, na.rm = TRUE ) )
        stop( '`r` must be <= 1!' )
    if ( length( r ) > 1 && length( n ) > 1 && length( r ) != length( n ) )
        stop( '`r` and `n` must have the same length if both are vectors!' )
    
    # use Fisher's transformation to estimate confidence intervals
    z <- ( log( 1 + r ) - log( 1 - r ) ) / 2
    z_alpha <- stats::qnorm( alpha / 2, lower.tail = FALSE )
    # CIs with small sample sizes will be set to NA
    z_alpha <- z_alpha / sqrt( ifelse( n < ci_offset, NA, n - ci_offset ) )
    # alpha confidence intervals
    CIL <- tanh( z - z_alpha )
    CIU <- tanh( z + z_alpha )
    
    return( cbind( r, CIL, CIU ) )
}
