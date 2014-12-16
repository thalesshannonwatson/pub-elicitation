args <- commandArgs(trailingOnly=TRUE)

source('lib/rnd-beta.R')

x <- seq(0, 1, by=0.01)

pdf(args[1])
plot(NA, xlim = c(0, 1), ylim = c(0, 8), xlab="Partial utility", ylab="Density")
for (i in 1:length(a)) { lines(x, dbeta(x, a[i], b[i])) }
dev.off()
