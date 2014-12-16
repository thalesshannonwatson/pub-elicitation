N.planes <- seq(1000, 50000, by=1000) # subsets to try

data <- lapply(3:12, function(i) {
  N.chunks <- 100
  N.per.chunk <- 1E4
  N.total <- N.chunks * N.per.chunk

  entropies <- matrix(NA, nrow=N.total, ncol=3, dimnames=list(NULL, c('h1','h2','h')))
  for (j in 1:N.chunks) {
    startIdx <- (j - 1) * N.per.chunk + 1
    fname <- paste0('data/convergence-entropies.n', i, '.', j, '.rds')
    entropies[startIdx:(startIdx+N.per.chunk-1),] <- readRDS(fname)
  }

  do.call(rbind, lapply(N.planes, function(n) {
    idx <- seq(0, N.total, by=n)
    idx <- cbind(idx[1:length(idx)-1] + 1, idx[2:length(idx)])
    cbind(n=rep(n,nrow(idx)), h=apply(idx, 1, function(subseq) {
      min(entropies[subseq[1]:subseq[2], "h"])
    }))
  }))
})

for (i in 1:length(data)) {
  data[[i]] <- cbind(data[[i]], 1/data[[i]][,"n"]^(1/5), rep(i+2, nrow(data[[i]])))
  colnames(data[[i]])[3] <- "ninv"
  colnames(data[[i]])[4] <- "dim"
}

data.all <- do.call(rbind, data)
data.all <- as.data.frame(data.all)
data.all$dim.cat <- as.factor(data.all$dim)
data.all$dim.ninv <- data.all$dim * data.all$ninv
lmfit <- lm(h ~ 0 + dim.cat + dim.ninv, as.data.frame(data.all))

pdf('../graphics/convergence.pdf')

par(mfrow=c(3,3))

for (i in 4:12) {
  data.this <- data[[i-2]]
  boxplot(h~n, data=data.this, sub=paste("n =", i), ylim=c(0.99*min(data.this[,'h']),max(data.this[,'h'])))
  prediction <- sapply(N.planes, function(n) {
    predict(lmfit, newdata=data.frame(dim.cat=factor(as.character(i), levels=levels(data.all$dim.cat)), dim.ninv=i/n^(1/5), ninv=1/n^(1/5)), interval="prediction")
  })
  lines(1:50, prediction[1,], col=2)
  lines(1:50, prediction[2,],lty=2, col=2)
  lines(1:50, prediction[3,],lty=2, col=2)
}

dev.off()

iterations <- function(n, target) {
  beta <- unname(lmfit$coefficients['dim.ninv'])
  (beta * n / target)^5
}

n <- 3:12
plot(n, iterations(n, 0.08), type='l', lty=2, xlab="Problem dimension", ylab="Iterations required")
lines(n, iterations(n, 0.1))
