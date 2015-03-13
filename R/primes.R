## Function to find the first n primes
primes <- function(n) { 
    peven <- 2 # seed list of even primes
    podd <- 3  # seed list of odd primes
    count <- 2 # count of primes identified so far

    ## Function to check a single number, p, against the current list of
    ## primes in podd. p is assumed to be odd. Returns TRUE if nothing in
    ## podd divides p; otherwise, returns FALSE
    isprime <- function(p) {
        i <- 1
        check <- TRUE
        lim <- sqrt(p)
        while( check & i <= length(podd) & podd[i] <= lim) {
            check <- p %% podd[i] != 0
            i <- i+1
        }
        check
    }

    while (count < n) {
        knownprimes <- podd
        foundprimes <- NULL

        m <- max(knownprimes)
        testseq <- seq(m+2, m**2-2, 2)
        j <- 1

        while (count < n & j <= length(testseq)) { 
            if ( isprime(testseq[j]) ) {
                foundprimes <- c(foundprimes, testseq[j])
            }
            j <- j+1
            count <- length(knownprimes) + length(foundprimes) + 1
            ## Have to add 1 to account for 2
        }
        podd <- c(podd, foundprimes)
    }

    c(peven, podd)
}