source('lib/code.R')
source('lib/rnd-problem.R')
library(smaa)

args <- commandArgs(trailingOnly=TRUE)
n <- as.integer(args[3])
k <- as.integer(args[4])

nr.planes <- 1E4

opts <- list(cuts=10, planes=nr.planes)

int.seed <- as.integer(args[2])
my.seed <- n * 1000 + k * 100 + int.seed

set.seed(my.seed)

meas <- gen.problem(n=n)

true.w <- simplex.sample(n, 1)$samples

## sanity check
stopifnot (k <= n)

sample.planes <- if (k == n) sample.planes.unrestricted.shakeandbake() else sample.planes.restricted.shakeandbake(k)

cuts <- get.cuts(true.w, opts$cuts, meas, opts$planes,
                 error.func = error.func.entropy(equal.w.prob=FALSE,
                   entropy=smaa.entropy.ranking),
                 sample.planes=sample.planes)

fname <- gsub(".rds", paste0(".n.", n, ".k.", k, ".seed.", args[2], ".rds"), args[1])

saveRDS(cuts, fname)
