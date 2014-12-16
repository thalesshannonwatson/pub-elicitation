source('lib/code.R')
#args <- commandArgs(trailingOnly=TRUE)

#set.seed(as.integer(args[2]))

N.total <- 1E6 # total number of planes to sample
N.planes <- seq(1000, 50000, by=1000) # subsets to try
# at the top level, we get 1E6/5E4 = 20 subsequences

thrombo <- readRDS('data/thrombo-problem.rds')
meas <- thrombo$meas
n <- dim(meas)[3]
constr <- list(constr=matrix(NA, nrow=0, ncol=n), dir=rep("<=", 0), rhs=rep(0, 0))

sample.planes <- sample.planes.unrestricted.shakeandbake()
entropy <- smaa.entropy.choice
error.func <- error.func.entropy(equal.w.prob=FALSE, entropy=entropy)

planes <- sample.planes(constr, N.total)$planes
print("Planes sampled")

best <- best.cutting.plane(constr, meas, planes, error.func=error.func)
print("Entropies estimated")

entropies <- best$entropies
result <- do.call(rbind, lapply(N.planes, function(n) {
  idx <- seq(0, N.total, by=n)
  idx <- cbind(idx[1:length(idx)-1] + 1, idx[2:length(idx)])
  cbind(n=rep(n,nrow(idx)), h=apply(idx, 1, function(subseq) {
    min(entropies[subseq[1]:subseq[2], "h"])
  }))
}))
boxplot(h~n, data=result)
abline(h=min(entropies[,"h"]))
abline(h=min(entropies[,"h"])*1.01, lty=2)
abline(h=min(entropies[,"h"])*1.02, lty=2)

#fname <- gsub(".rds", paste0(".seed.", args[2], ".rds"), args[1])

#saveRDS(cuts, fname)
