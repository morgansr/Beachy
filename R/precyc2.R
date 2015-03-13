## New seive using the recycling feature of R
precyc <- function (n) {
    odds <- seq(3,n,2)
    isprime <- rep(TRUE, length(odds))
    oddprimes <- NULL
    lim <- sqrt(n)
    i <- 1

    while ( odds[i] < lim ) {
        while ( !isprime[i] ) {
            i <- i + 1
        }
        oddprimes <- c(oddprimes, odds[i])
        strainer <- c(rep(TRUE, i-1), FALSE, rep(TRUE, odds[i]-i))
        suppressWarnings(
            isprime <- isprime & strainer
        )
    }

    c(2, oddprimes, odds[isprime])
}