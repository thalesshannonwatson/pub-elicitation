library(smaa)

N <- 1E4
meas <- array(NA, dim=c(N, 2, 2), dimnames=list(NULL, c("A","B"), c("u", "v")))
meas[,1,1] <- runif(N, 0.0, 0.3)
meas[,1,2] <- runif(N, 0.7, 1.0)
meas[,2,1] <- runif(N, 0.3, 0.6)
meas[,2,2] <- runif(N, 0.2, 0.4)

x <- seq(0, 1, by=0.01)

v <- sapply(x, function(w) { apply(smaa.values(meas, cbind(rep(w, N), rep(1-w, N))), 2, median) })
v.low <- sapply(x, function(w) { apply(smaa.values(meas, cbind(rep(w, N), rep(1-w, N))), 2, quantile, 0.025) })
v.upp <- sapply(x, function(w) { apply(smaa.values(meas, cbind(rep(w, N), rep(1-w, N))), 2, quantile, 0.975) })

p.a <- sapply(x, function(w) { unname(smaa(meas, cbind(rep(w, N), rep(1-w, N)))$ra[1,1]) })
p.b <- sapply(x, function(w) { unname(smaa(meas, cbind(rep(w, N), rep(1-w, N)))$ra[2,1]) })
h.w <- sapply(x, function(w) { smaa.entropy.choice(smaa(meas, cbind(rep(w, N), rep(1-w, N)))$ra) })

entropy <- function(w1) {
  smaa.entropy.choice(smaa(meas, cbind(w1, 1 - w1))$ra)
}

h <- sapply(x, function(w) {
  w1 <- runif(N, 0.0, w)
  h1 <- entropy(w1)
  w2 <- runif(N, w, 1.0)
  h2 <- entropy(w2)
  c(h1=h1, h2=h2, h=w * h1 + (1 - w) * h2)
})

h1 <- h["h1",]
h2 <- h["h2",]
h <- h["h",]


h.nopref <- entropy(runif(N, 0, 1))

pdf("../graphics/example.pdf")

par(mfrow=c(2,2))

plot(x, v[1,], type='l', ylim=c(0,1), xlab=expression(w[1]), ylab="Value", main="Utilities (fixed weight)")
lines(x, v[2,])
lines(x, v.low[1,], lty=3)
lines(x, v.low[2,], lty=3)
lines(x, v.upp[1,], lty=3)
lines(x, v.upp[2,], lty=3)

plot(x, h.w, ylim=c(0,1), type='l', xlab=expression(w[1]), ylab="Metric", main="Metrics (fixed weight)")
lines(x, p.a, lty=2)
lines(x, p.b, lty=3)
legend("left", lty=1:3, legend=c("Entropy", "P(A best)", "P(B best)"))

plot(smooth.spline(x, h1), ylim=c(0,1), type='l', xlab="x", ylab="Entropy", lty=2, main="Decision entropy (bounded region)")
lines(smooth.spline(x, h2), lty=4)
abline(h=h.nopref, lty=3)
legend("left", lty=c(3,2,4), legend=c(expression('0 <' ~ w[1] ~ '< 1'),
                                 expression('0 <' ~ w[1] ~ '< x'),
                                 expression('x <' ~ w[1] ~ '< 1')))

plot(smooth.spline(x,h), ylim=c(0,1), type='l', xlab="x", ylab="Entropy", main="Conditional entropy")
lines(smooth.spline(x, h1 * x), lty=3)
lines(smooth.spline(x, h2 * (1 - x)), lty=3)
abline(v=x[which.min(h)], lty=2)

dev.off()
