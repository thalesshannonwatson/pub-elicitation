source('lib/code.R')
source('lib/rnd-problem.R')
library(plyr)

args <- commandArgs(trailingOnly=TRUE)
print(args)
n <- as.integer(args[2])
seed <- as.integer(args[3])
m <- 10 # 10 alts
opts <- list(cuts=1, planes=1E4) # only 1 cut

message ("Running time tests with n=", n, " seed=", seed)

## Function needed to generate pareto-optimal alternatives
randomPointFromHypersphere <- function(ncrit) {
  rns <- c()
  while(TRUE) {
    rns <- rnorm(ncrit)
    if (all(rns > 0)) {
      break
    }
  }
  mul <- 1 / sqrt(sum(rns * rns))
  return(rns * mul)
}

meas <- gen.problem(n=n, m=8, N=opts$planes)
true.w <- simplex.sample(n, 1)$samples

cuts <- get.cuts(true.w, opts$cuts, meas, opts$planes,
                 error.func = error.func.entropy(equal.w.prob=FALSE,
                   entropy=smaa.entropy.ranking),
                 sample.planes=sample.planes.unrestricted.shakeandbake(),
                 cluster.size=1) # no parallel processing

fname <- gsub(".rds", paste0(".n.", n, ".seed.", seed, ".rds"), args[1])

saveRDS(cuts, fname)
