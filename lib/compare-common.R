do.compare <- function(nr.planes, error.function) {
  source('lib/code.R')
  source('lib/rnd-problem.R')
  library(smaa)

  calc.entropies <- function(cuts) {
    n <- length(cuts[[1]]$point)
    entropy.fn <- error.func.entropy(equal.w.prob=FALSE, entropy=smaa.entropy.ranking)
    constr <- list(constr=matrix(NA, nrow=0, ncol=n), dir=rep("<=", 0), rhs=rep(0, 0))
    entropies <- list()
    for (cut in cuts) {
      w <- harSample(constr, N)
      entropies[[length(entropies) + 1]] <- entropy.fn(cut, meas, constr, w)
      constr <- eliminateRedundant(mergeConstraints(
                  constr,
                  plane.constraint(
                      cut$point, cut$normal,
                      plane.side(true.w, cut$point, cut$normal))))
    }
    entropies
  }

  args <- commandArgs(trailingOnly=TRUE)
  N <- 1E4
  int.seed <- as.integer(args[3])
  n <- as.integer(args[2])

  opts <- list(cuts=10, planes=nr.planes)

  my.seed <- n * 1013 * int.seed

  set.seed(my.seed)
  meas <- gen.problem(n=n)
  true.w <- simplex.sample(n, 1)$samples

  cuts <- get.cuts(true.w, opts$cuts, meas, opts$planes,
                   error.func = error.function,
                   sample.planes=sample.planes.unrestricted.shakeandbake(),
                   stopping.entropy=NULL)
  entropies <- calc.entropies(cuts)

  fname <- gsub(".rds", paste0(".n.", n, ".seed.", int.seed, ".rds"), args[1])

  saveRDS(list(cuts=cuts, entropies=entropies), fname)
}
