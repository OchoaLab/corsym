# vectorized!  returns matrix!
# default to Pearson's offset
# rs that are NA propagate, and otherwise return CIs can be NA when sample size is too small or NA
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
    
    # use Fisher's transformation to estimate confidence intervals
    z <- ( log( 1 + r ) - log( 1 - r ) ) / 2
    rad <- stats::qnorm( alpha / 2, lower.tail = FALSE )
    # CIs with small sample sizes will be set to NA
    rad <- rad / sqrt( ifelse( n < ci_offset, NA, n - ci_offset ) )
    # alpha confidence intervals
    CIL <- tanh( z - rad )
    CIU <- tanh( z + rad )
    
    return( cbind( r, CIL, CIU ) )
}
