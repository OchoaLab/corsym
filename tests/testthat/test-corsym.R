# simulate independent data for this test
n <- 13
x <- rnorm( n )
y <- rnorm( n )
# matrix version
X <- cbind( x, y )

# versions with missingness
xm <- x
ym <- y
xm[ c(1,3) ] <- NA
ym[ c(2,3) ] <- NA
Xm <- cbind( xm, ym )
# expected complete equivalents
xmc <- xm[ -(1:3) ]
ymc <- ym[ -(1:3) ]
nmc <- n-3 # only 3 pairs have NAs in the end

test_that( "partial_order works", {
    # cause errors on purpose
    expect_error( partial_order() )
    # input must be matrix, not vector
    expect_error( partial_order( x ) )

    # main run, results in extreme order
    # NOTE: use <<- to remember variable globally (outside this scope)
    expect_silent(
        Xe <<- partial_order( X )
    )
    # minimally, dimensions should match)
    expect_equal( dim( Xe ), dim( X ) )
    # confirm that every row is increasing, as advertised
    expect_true( all( apply( Xe, 1, diff ) >= 0 ) )

    # test a random partial order
    f <- runif( 1 )
    expect_silent(
        Xp <<- partial_order( X, f )
    )
    expect_equal( dim( Xp ), dim( X ) )
    # we don't know here which rows were reordered, so we can't test them specifically
    # but we do expect to see the effect on the averages (the first column should have lower values, and the second column higher values, than the starting data)
    expect_true( mean( X[,1] ) >= mean( Xp[,1] ) )
    expect_true( mean( X[,2] ) <= mean( Xp[,2] ) )

    # edge case of no reordering, it should not change data
    expect_silent(
        X0 <- partial_order( X, 0 )
    )
    expect_equal( X0, X )
})

validate_handle_args <- function( obj, x, y, n ) {
    expect_true( is.list( obj ) )
    expect_equal( names( obj ), c('x', 'y', 'n') )
    # when there's no missingness, this just returns these input values
    expect_equal( obj$x, x )
    expect_equal( obj$y, y )
    expect_equal( obj$n, n )
}

test_that("handle_args works", {
    # cause failures on purpose
    # need at least one arg
    expect_error( handle_args( ) )
    # and the one arg needs to be a matrix
    expect_error( handle_args( x ) )
    # two vectors need to have same length
    expect_error( handle_args( x, y[-1] ) )
    
    # test version with two vectors
    expect_silent(
        obj <- handle_args( x, y )
    )
    validate_handle_args( obj, x, y, n )

    # test matrix version
    expect_silent(
        obj <- handle_args( X )
    )
    validate_handle_args( obj, x, y, n )

    # versions with missingness
    expect_silent(
        obj <- handle_args( xm, ym )
    )
    validate_handle_args( obj, xmc, ymc, nmc )
    expect_silent(
        obj <- handle_args( Xm )
    )
    validate_handle_args( obj, xmc, ymc, nmc )
})

test_that( "add_cor_CI works", {
    # cause errors on purpose
    # first two arguments are mandatory
    expect_error( add_cor_CI() )
    expect_error( add_cor_CI( 0 ) )
    expect_error( add_cor_CI( n=1 ) )
    # invalid ranges
    expect_error( add_cor_CI( -1.1, n ) )
    expect_error( add_cor_CI( 1.1, n ) )
    
    # draw m random correlations between -1 and 1
    m <- 17
    rs <- runif( m, min = -1 )
    expect_silent(
        out <- add_cor_CI( rs, n )
    )
    expect_true( is.numeric( out ) )
    expect_true( is.matrix( out ) )
    expect_equal( ncol( out ), 3 )
    expect_equal( nrow( out ), m )
    expect_true( all( rs == out[,1] ) )
    expect_true( all( rs >= out[,2] ) )
    expect_true( all( rs <= out[,3] ) )

    # two edge cases, they behave perfectly!
    expect_silent(
        out <- add_cor_CI( c(1,-1), n )
    )
    out_exp <- rbind( c(1,1,1), -c(1,1,1) )
    colnames( out_exp ) <- c('r', 'CIL', 'CIU')
    expect_equal( out, out_exp )
    
    # low sample sizes (relative to ci_offset = 3) have a special behavior
    expect_silent(
        out <- add_cor_CI( 0, 2:3 )
    )
    out_exp <- cbind( 0, c(NA, -1), c(NA, 1) )
    colnames( out_exp ) <- c('r', 'CIL', 'CIU')
    expect_equal( out, out_exp )

})

test_that( "pearson works", {
    # cause errors on purpose
    expect_error( pearson( ) )
    expect_error( pearson( x ) )
    expect_error( pearson( x, y[-1] ) )

    # basic test on random data
    expect_silent(
        out <- pearson( x, y )
    )
    expect_true( is.numeric( out ) )
    expect_equal( length( out ), 3 )
    expect_true( out[1] >= out[2] )
    expect_true( out[1] <= out[3] )

    # same calculation but with matrix input
    expect_silent(
        out2 <- pearson( X )
    )
    expect_equal( out2, out )

    # turns out small sample sizes don't cause errors with pearson
    expect_silent(
        out <- pearson( 1, 1 )
    )
    # dumb hack to make values numeric
    expect_equal( out, 0+c(NA, NA, NA) )
    expect_silent(
        out <- pearson( 1:2, 1:2 )
    )
    expect_equal( out, c(1, NA, NA) )
    expect_silent(
        out <- pearson( 1:3, 1:3 )
    )
    expect_equal( out, c(1, NaN, 1) )

    # confirm behavior when there's missingness
    expect_silent(
        out <- pearson( xm, ym )
    )
    expect_equal( out[1], cor( xm, ym, use = 'complete.obs' ) )
    expect_silent(
        out2 <- pearson( Xm )
    )
    expect_equal( out2, out )
})

test_that( "corsym works", {
    # cause errors on purpose
    expect_error( corsym( ) )
    expect_error( corsym( x ) )
    expect_error( corsym( x, y[-1] ) )

    # basic test on random data
    expect_silent(
        out <- corsym( x, y )
    )
    expect_true( is.numeric( out ) )
    expect_equal( length( out ), 3 )
    expect_true( out[1] >= out[2] )
    expect_true( out[1] <= out[3] )

    # same calculation but with matrix input
    expect_silent(
        out2 <- corsym( X )
    )
    expect_equal( out2, out )
    # here we expect the exact same outputs on the ordered data
    # test extreme and partial orders, which were precalculated
    expect_silent(
        out2 <- corsym( Xe )
    )
    expect_equal( out2, out )
    expect_silent(
        out2 <- corsym( Xp )
    )
    expect_equal( out2, out )

    # repeat with missingness
    expect_silent(
        out <- corsym( xm, ym )
    )
    expect_true( is.numeric( out ) )
    expect_equal( length( out ), 3 )
    expect_true( out[1] >= out[2] )
    expect_true( out[1] <= out[3] )
    expect_silent(
        out2 <- corsym( Xm )
    )
    expect_equal( out2, out )

    # turns out small sample sizes don't cause errors with corsym
    expect_silent(
        out <- corsym( 1, 1 )
    )
    # dumb hack to make values numeric
    expect_equal( out, 0+c(NA, NA, NA) )
    # corsym has non-NA CIs for two samples (because ci_offset=1.5)
    expect_silent(
        out <- corsym( 1:2, 1:2 )
    )
    expect_equal( out, c(1, 1, 1) )
})
