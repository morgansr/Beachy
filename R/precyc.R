## New seive using the recycling feature of R
precyc <- function (n) {
    odds <- seq(3,n,2)
    isprime <- rep(TRUE, length(odds))
    lim <- sqrt(n)
    i <- 1

    while ( odds[i] < lim ) {
        while ( !isprime[i] ) {
            i <- i + 1
        }
        strainer <- c(FALSE, rep(TRUE, odds[i]-1))
        suppressWarnings(
            isprime <- c(
                isprime[1:(i+odds[i]-1)],
                isprime[-(1:(i+odds[i]-1))] & strainer
            )
        )
        i <- i+1
    }

    c(2, odds[isprime])
}