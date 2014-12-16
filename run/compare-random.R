source('lib/compare-common.R')
nr.planes <- 1E4 # get.cuts won't handle < 2
do.compare(nr.planes, function(cut, meas, constr, w) { x <- runif(n=1); list(h1=x, h2=x, h=x) })
