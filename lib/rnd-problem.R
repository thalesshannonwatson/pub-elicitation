gen.problem <- function(n, m=8, N=1E4) {
    meas <- array(NA, dim=c(N, m, n))
    
    for (j in 1:n) {
        source('lib/rnd-beta.R')
        ## choose distribution for each criterion
        d <- sample.int(length(a), m)
        ## sample measurements (alternative i, criterion j)
        for (i in 1:m) {
            meas[ , i, j] <- rbeta(N, a[d[i]], b[d[i]])
        }
    }
    meas
}
