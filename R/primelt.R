## Function to find the primes less than n, where n > 3
primeslt <- function (n) {
    testseq <- seq(3, n, 2)
    isprime <- rep(TRUE, length(testseq))
    i <- 1
    p <- 3
    lim <- sqrt(n)

    while ( p < lim ) {
        
        while ( !isprime[i] ) {
            i <- i + 1
        }
        j <- i
        p <- testseq[j]
        while ( j < n/2 - p ) {
            j <- j + p
            isprime[j] <- FALSE
        }
        i <- i+1
    }
    
    c(2,testseq[isprime])
}
