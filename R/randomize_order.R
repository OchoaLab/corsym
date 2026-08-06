#' Randomize ordering of each row of a matrix
#'
#' This function shuffles the values in each row, so they appear in a random order.
#'
#' @param X Matrix of data.
#' 
#' @return The matrix with randomized rows.
#'
#' @examples
#' # dummy data in order
#' X <- matrix( 1:20, nrow = 10, ncol = 2, byrow = TRUE )
#'
#' # same data but with each row in random order
#' X_randomized <- randomize_order( X )
#'
#' @export
randomize_order <- function( X ) {
    # validations
    if ( missing( X ) )
        stop( '`X` is required!' )
    if ( !is.matrix( X ) )
        stop( '`X` must be a matrix!' )

    # this is all we needed!
    t( apply( X, 1, sample ) )
}
