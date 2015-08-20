library(plyr)
source('lib/code.R')

## Computes uncertainty coefficient
comp.r <- function(test) {
    (test$stopping$h - mean(test$stopping$all.ent)) / test$stopping$h
}

args <- commandArgs(trailingOnly=TRUE)

seed.rng <- 1:20
k.rng <- 2:10
plot.vals <- c(3, 5, 7, 8, 9)

read.data <- function(n, k, seed) {
    seedInt = seed + (10 * n + k)
    readRDS(paste0("data/random-restricted.n.", n, ".k.", k, ".seed.", seedInt, ".rds"))
}

uc.all <- laply(1:length(plot.vals), function(ind) {
  k <- plot.vals[ind]
  res <- alply(seed.rng, 1, read.data, n=10, k=k)
  laply(res, function(cuts) {laply(cuts, comp.r)}, .drop=FALSE)
})

uc.avgs <-  aaply(uc.all, 1, colMeans)

pdf(args[1])

plot(NULL, xlim=c(1, 10), ylim=c(0.1, 0.3), main="n=10", xlab="Question", ylab="avg(R)")
matlines(t(uc.avgs), type='l', col='black')

legend(legend=alply(plot.vals, 1, function(n) {paste0('k=', n)}),
       x='topright', lty=1:length(plot.vals), cex=1.0)

dev.off()

