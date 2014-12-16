par(mfrow=c(3,3))

for (i in 4:12) {
  N.chunks <- 100
  N.per.chunk <- 1E4
  N.total <- N.chunks * N.per.chunk

  entropies <- matrix(NA, nrow=N.total, ncol=3, dimnames=list(NULL, c('h1','h2','h')))
  for (j in 1:N.chunks) {
    startIdx <- (j - 1) * N.per.chunk + 1
    fname <- paste0('data/convergence-entropies.n', i, '.', j, '.rds')
    entropies[startIdx:(startIdx+N.per.chunk-1),] <- readRDS(fname)
  }

  N.planes <- seq(1000, 50000, by=1000) # subsets to try

  result <- do.call(rbind, lapply(N.planes, function(n) {
    idx <- seq(0, N.total, by=n)
    idx <- cbind(idx[1:length(idx)-1] + 1, idx[2:length(idx)])
    cbind(n=rep(n,nrow(idx)), h=apply(idx, 1, function(subseq) {
      min(entropies[subseq[1]:subseq[2], "h"])
    }))
  }))
  boxplot(h~n, data=result, sub=paste("n =", i))
  abline(h=min(entropies[,"h"]))
  abline(h=min(entropies[,"h"])*1.01, lty=2)
  abline(h=min(entropies[,"h"])*1.02, lty=2)
}
