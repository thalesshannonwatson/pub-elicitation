library(plyr)
library(hitandrun)
library(smaa)
source('lib/rnd-problem.R')
source('lib/code.R')

N <- 1E4
dims <- c(3, 6, 9)
seeds <- 1:20
methods <- c('entropy', 'volume', 'random')

load.n <- function(method, n) {
    alply(seeds, 1, function(seed) {
        readRDS(paste0('data/compare-', method, '.n.', n, '.seed.', seed, '.rds'))
    })
}

get.entropies <- function(data) {
    x <- llply(data, with, entropies)
    llply(x, function(l) {matrix(unlist(l), byrow=TRUE, ncol=3)})
}

get.h <- function(x) {
    t(laply(x, function(r) {r[,3]}))
}

do.plot.avgs <- function(n, min.h) {
    n.min.h <- as.numeric(min.h[as.character(n),])
    data <- llply(methods, load.n, n)
    all.entropies <- llply(data, get.entropies)
    h <- llply(all.entropies, get.h)
    diff.h <- llply(h, aaply, 1, '-', n.min.h)
    mean.diff <- t(laply(diff.h, rowMeans))

    matplot(mean.diff, type='l', main=paste0('n=', n), ylab=expression(avg(h) - h[min]), xlab='Q')
    legend('topright', methods, lty=1:3, col=1:3)
}

do.plot.box <- function(method, n, min.h) {
    n.min.h <- as.numeric(min.h[as.character(n),])
    data <- load.n(method, n)
    h <- get.h(get.entropies(data))
    h.diff <- aaply(h, 1, '-', n.min.h)
    boxplot(t(h.diff), main=paste0('n=', n, ', ', method), ylim=c(-1.5, 5), xlab='Q', ylab=expression(h - h[min]))
}

problem.min.entropy <- function(n, seed) {
  my.seed <- n * 1013 * seed
  set.seed(my.seed)
  meas <- gen.problem(n=n)
  true.w <- simplex.sample(n, 1)$samples
  constr <- list(constr=matrix(NA, nrow=0, ncol=n), dir=rep("<=", 0), rhs=rep(0, 0))
  w <- matrix(rep(true.w, N), ncol=n, byrow=TRUE)
  smaa.entropy.ranking(smaa.ranks(smaa.values(meas, w)))
}

min.entropies <- function() {
    pars <- expand.grid(dims, seeds)
    colnames(pars) <- c('n', 'seed')
    as.data.frame(maply(pars, problem.min.entropy))
}

min.h <- min.entropies()

pdf('../graphics/comparison.pdf')

par(mfrow=c(2, 3))

alply(dims, 1, do.plot.avgs, min.h)
llply(methods, do.plot.box, 9, min.h)

dev.off()
