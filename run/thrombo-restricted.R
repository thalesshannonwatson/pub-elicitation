source('lib/code.R')
source('lib/plots.R')

set.seed(1911)

k <- 2

thrombo <- readRDS('data/thrombo-problem.rds')
opts <- dget('run/thrombo-opts.R')

cuts <- get.cuts(thrombo$w.exact, opts$cuts, thrombo$meas, opts$planes,
                 error.func = error.func.entropy(equal.w.prob=FALSE,
                   entropy=smaa.entropy.choice),
                 sample.planes=sample.planes.restricted.shakeandbake(k))

saveRDS(cuts, 'data/thrombo-restricted.rds')

