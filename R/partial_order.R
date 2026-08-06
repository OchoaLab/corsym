#' Apply partial ordering to the rows of a matrix
#'
#' This function reorders a proportion of rows so values appear in increasing order.
#'
#' @param X Matrix of data.
#' @param f Proportion of rows to force to have extreme order (sorted in increasing order).
#' 
#' @return The input matrix with a random proportion `f` of rows sorted in increasing order, the rest equals the input data.
#'
#' @examples
#' # dummy independent data in random order
#' X <- matrix( rnorm( 20 ), nrow = 10, ncol = 2 )
#'
#' # same data but in extreme order by default
#' X_extreme <- partial_order( X )
#' 
#' # apply a partial ordering
#' X_partial <- partial_order( X, 0.5 )
#'
#' @export
partial_order <- function( X, f = 1 ) {
    # validations
    if ( missing( X ) )
        stop( '`X` is required!' )
    if ( !is.matrix( X ) )
        stop( '`X` must be a matrix!' )
    if ( f < 0 )
        stop( '`f >= 0` is required!' )
    if ( f > 1 )
        stop( '`f <= 1` is required!' )

    # handle edge case where we wanted to keep original order
    if ( f == 0 )
        return( X )

    # else f>0
    # easiest to construct extreme order first
    Xe <- t( apply( X, 1, sort ) )

    # extreme order case
    if ( f == 1 )
        return( Xe )

    # what is left is partial order, do more work...
    # randomly select rows to force extreme order on
    n <- nrow( X )
    indexes <- sample.int( n, n * f )
    # overwrite original rows with extreme order ones, on that row subset only
    X[ indexes, ] <- Xe[ indexes, ]
    # return edited matrix!
    return( X )
}
