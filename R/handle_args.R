# shared by corsym and pearson
handle_args <- function( x, y = NULL ) {
    # validate inputs
    if ( missing( x ) )
        stop( '`x` is required!' )
    if ( is.null( y ) ) {
        # in this mode, x must be a matrix with 2 columns
        if ( !is.matrix( x ) )
            stop( 'When a single argument `x` is provided, it must be a matrix!' )
        if ( ncol( x ) != 2 )
            stop( '`x` must have 2 columns!' )

        # exclude pairs with any missingness
        if ( anyNA( x ) )
            x <- x[ rowSums( is.na( x ) ) == 0, ]

        # count pairs now (after removing missing cases)
        n <- nrow( x )
        # extract the two vectors of interest
        y <- x[ , 2 ]
        x <- x[ , 1 ]
    } else {
        # in this case both need to be vectors of equal length
        # make sure length is equal
        n <- length( x )
        if( length( y ) != n )
            stop( 'Both inputs must have the same length!' )

        # now do something about missingness
        if ( anyNA( x ) || anyNA( y ) ) {
            indexes <- is.na( x ) + is.na( y ) == 0
            x <- x[ indexes ]
            y <- y[ indexes ]
            # recalculate length
            n <- length( x )
        }
    }
    return( list( x = x, y = y, n = n ) )
}
