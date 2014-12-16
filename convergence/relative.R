data <- sapply(4:12, function(i) {
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
    mean(apply(idx, 1, function(subseq) {
      min(entropies[subseq[1]:subseq[2], "h"])
    }))
  }))
})

for (i in 1:9) { data[, i] <- data[, i] / data[50, i] }

par(mfrow=c(2,2))

plot(NA, xlim=c(1E3,5E4), ylim=c(1,1.05), ylab="Entropy/Entropy after 50k", xlab="Iterations")
for (i in 1:9) { lines((1:50)*1000, data[, i]) }

plot(NA, xlim=c(1E3,5E4), ylim=c(1,1.05), ylab="Entropy/Entropy after 50k", xlab="Iterations (log)", log="x")
for (i in 1:9) { lines((1:50)*1000, data[, i]) }

plot(NA, xlim=c(1/5E4^(1/4),1/1E3^(1/4)), ylim=c(1,1.05), ylab="Entropy/Entropy after 50k", xlab="1/Iterations^(1/4)")
for (i in 1:9) { lines(1/((1:50)*1000)^(1/4), data[, i]) }

plot(NA, xlim=c(1/5E4^(1/5),1/1E3^(1/5)), ylim=c(1,1.05), ylab="Entropy/Entropy after 50k", xlab="1/Iterations^(1/5)")
for (i in 1:9) { lines(1/((1:50)*1000)^(1/5), data[, i]) }

par(mfrow=c(2,2))
for (i in c(1,5,10,20)) {
  plot(4:12, data[i,], xlab="n", ylab="Entropy/Entropy after 50k", ylim=c(1,1.05), main=paste0("After ", i, "k iterations"))
}
