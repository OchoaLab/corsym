# same interface and missingness handling as corsym, adds CIs unlike default `cor`
#' @export
pearson <- function( x, y = NULL, alpha = 0.05, ci_offset = 3 ) {
    # normalize inputs
    obj <- handle_args( x, y )
    x <- obj$x
    y <- obj$y
    n <- obj$n

    # calculate default correlation
    r <- cor( x, y )
    # use external function to add CIs
    # flatten to vector
    return( as.numeric( add_cor_CI( r, n, alpha = alpha, ci_offset = ci_offset ) ) )
}
