source('lib/compare-common.R')
source('lib/code.R')
nr.planes <- 1E4
do.compare(nr.planes, error.func.entropy(equal.w.prob=FALSE, entropy=smaa.entropy.ranking))
