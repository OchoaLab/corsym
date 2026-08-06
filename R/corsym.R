# implementation of the method proposed in this paper

#' @export
corsym <- function( x, y = NULL, alpha = 0.05, ci_offset = 1.5 ) {
    # normalize inputs
    obj <- handle_args( x, y )
    x <- obj$x
    y <- obj$y
    n <- obj$n
    
    # n only appears as 2*n...
    nn <- 2 * n
    
    # estimate pooled mean
    # this uses a bit less memory than concatenating x and y first
    mu <- ( mean( x ) + mean( y ) ) / 2
    # now estimate "base" pooled variance
    sigma_sq <- ( sum( ( x - mu )^2 ) + sum( ( y - mu )^2 ) ) / ( nn - 1 )
    # and "base" covariance estimate
    covar <- sum( ( x - mu ) * ( y - mu ) ) * 2 / ( nn - 1 )

    # now form small sample unbiased estimates
    an <- ( nn - 1 ) ^2 / ( nn * ( nn - 2 ) )
    bn <- an / ( nn - 1 )
    sigma_sq_u <- an * sigma_sq + bn * covar
    covar_u <- an * covar + bn * sigma_sq

    # finally, estimate correlation and return!
    r <- covar_u / sigma_sq_u
    # use external function to add CIs
    # flatten to vector
    return( as.numeric( add_cor_CI( r, n, alpha = alpha, ci_offset = ci_offset ) ) )
}

