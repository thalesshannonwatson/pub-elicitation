source('lib/code.R')
source('lib/hyperplane.sample.shakeandbake.R')

args <- commandArgs(trailingOnly=TRUE)

set.seed(as.integer(args[2]))

thrombo <- readRDS('data/thrombo-problem.rds')
opts <- dget('run/thrombo-opts.R')

cuts <- get.cuts(thrombo$w.exact, opts$cuts, thrombo$meas, opts$planes,
                 error.func = error.func.entropy(equal.w.prob=FALSE,
                     entropy=smaa.entropy.choice),
                 sample.planes=sample.planes.unrestricted.shakeandbake())

fname <- gsub(".rds", paste0(".seed.", args[2], ".rds"), args[1])

saveRDS(cuts, fname)
