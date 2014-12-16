library(sfsmisc)
library(plyr)

crit <- 3:10
seeds <- 1:20

get.times <- function(seed, n) {
    data <- readRDS(paste0('data/timing-test.n.', n, '.seed.', seed, '.rds'))
    run.times <- matrix(data[[1]]$time, ncol=3)
    colnames(run.times) <- c('sampling', 'best', 'stopping')
    run.times
}

n.times <- function(n) {
    rtimes <- alply(seeds, 1, get.times, n)
    do.call(rbind, rtimes)
}

all.times <- alply(crit, 1, n.times)
mean.times <- llply(all.times, colMeans)
names(mean.times) <- crit
mean.times <- do.call(rbind, mean.times)
mean.times <- mean.times[,c(1,3,2)] # reorder

pdf('../graphics/timing.pdf')

barplot(t(mean.times / 60), log='y', legend=c('plane sampling',
                                         'stopping criterion',
                                         'best plane estimation'
                                         ),
        args.legend=list('x'='topleft', bty='n'),
        ylab='min', xlab='n', axes=FALSE
        )
aY <- axTicks(2)
axis(2, at=aY, label= axTexpr(2, aY), las=2)

dev.off()
