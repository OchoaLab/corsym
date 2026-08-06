# CorSym: Correlation Estimation For Exchangeable/Symmetrical Variables

<!-- badges: start -->
<!-- badges: end -->

This package implements CorSym, an unbiased estimator of the correlation for exchangeable data, where the values (x,y) in a pair have no inherent order, so replacing with (y,x) randomly in any or all pairs is equally valid.
This scenario occurs frequently in the study of assortative pairing, for example, what is the correlation between the two incomes of a couple, or their ancestry proportions?
It also provides functions to calculate confidence intervals (CIs) separately, modify variable orders, a statistical test for order bias, and a wrapper around the standard Pearson estimator to illustrate its problematic dependence on data ordering (under the hypothesis that order is actually meaningless).

## Installation

<!--
You can install the released version of corsym from [CRAN](https://CRAN.R-project.org) with:
``` r
install.packages("corsym")
```
-->

You can install the development version of corsym from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("OchoaLab/corsym")
```

## Example

Suppose you have a matrix `X` with paired data (two columns), and the order within each pair is arbitrary.
For example, the proportion of the genome of each parent that is of a given ancestry, where different rows are different sets of parents.
We want to estimate the correlation between values in the pair.
The standard Pearson estimator can be biased if the data has an order bias, while CorSym is unbiased and gives the same answer for all orders.

``` r
library(corsym)

# Calculate correlations with CIs with both methods.
# If there was an order bias, they will disagree more,
# otherwise they might be close,
# but Pearson gives a different answer for each order!
pearson( X )
corsym( X )

# if your data is in random order, create a version with extreme order
# (every pair starts with the minimum value, ends with the maximum in the pair):
Xe <- partial_order( X )

# confirm Pearson gives a very different answer for extremely ordered data
# while CorSym gives exactly the same answer both ways
pearson( Xe )
corsym( Xe )

# test the data for order bias
# count the number of times the first value was the smaller one of the pair,
# separately for the original data and the extreme order data
data <- data.frame(
  x = c( sum( X[,1] < X[,2] ), sum( Xe[,1] < Xe[,2] ) ),
  n = n
)
# this function reports the frequency of those cases,
# and p-values under the null hypothesis that the true frequency is 0.5
order_bias_test( data )
```

