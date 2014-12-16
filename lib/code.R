library(doParallel)
library(smaa)
library(hitandrun)
library(parallel)
source('lib/partition.points.R')
source('lib/plane.R')
source('lib/hyperplane.sample.shakeandbake.R')

comp.uc <- function(nalts, stopping) {
    norm <- log2(nalts) # bits of information, choice problem
    (stopping$h - stopping$h.min) / norm
}

## Sample N points in n-dimensions, within simplex with additional constraints
harSample <- function(constr, N) {
  n <- ncol(constr$constr)
  constr <- eliminateRedundant(mergeConstraints(constr, simplexConstraints(n)))
  hitandrun(constr, N)
}

find.cut <- function(meas, nrPlanes, prevCuts, error.func, sample.planes, cluster.size=detectCores(), stopping.entropy=smaa.entropy.ranking) {
  N <- dim(meas)[1]
  n <- dim(meas)[3]

  cl <- makeCluster(cluster.size)
  registerDoParallel(cl)
  
  ## Sample planes
  if (is.null(prevCuts)) {
    prevCuts <- list(constr=matrix(NA, nrow=0, ncol=n), dir=rep("<=", 0), rhs=rep(0, 0))
  }
  sampling.t <- system.time(
      planes <- sample.planes(prevCuts, nrPlanes)$planes
      )[3]
  message("- planes sampled in ", sampling.t, "s")

  plane.find.t <- system.time(
                              best <- best.cutting.plane(prevCuts, meas, planes, error.func=error.func)
      )[3]
  message("- best plane found in ", plane.find.t, "s")
  cutPt <- planes[[best$choice]]$point
  cutDir <- planes[[best$choice]]$normal

  pts <- harSample(prevCuts, N)

  stopping.t <- system.time(
      stopping <- if (!is.null(stopping.entropy)) { stopping.calc(meas, pts, stopping.entropy) } else NULL
      )[3]
  message("- stopping criterion computed in ", stopping.t, "s")
  
  partition <- partition.points(pts, cutPt, cutDir)
  values <- smaa.values(meas, pts)
  ranks <- smaa.ranks(values)

  stopCluster(cl)

  list(entropies=best$entropies, point=cutPt, normal=cutDir,
       share=sum(partition) / N, h=best$entropies[best$choice, "h"],
       ra=smaa.ra(ranks), stopping=stopping, time=c(sampling.t, plane.find.t, stopping.t))
}

# meas: measurements
# wn: weights
# entropy.fn: entropy calculation function
stopping.calc <- function(meas, wn, entropy.fn) {
  entropy <- function(w) {
    ranks <- smaa.ranks(smaa.values(meas=meas, pref=w))
    entropy.fn(ranks)
  }

  hn <- entropy(wn)
  all.ent <- aaply(wn, 1, entropy, .parallel=TRUE)
  i.min <- which.min(all.ent)
  i.max <- which.max(all.ent)
  list(h=hn,
       w.min=wn[i.min,],
       h.min=all.ent[i.min],
       w.max=wn[i.max,],
       h.max=all.ent[i.max],
       all.ent=all.ent)
}

get.cuts <- function(tgt, nr, meas, nrPlanes, error.func, sample.planes=sample.planes.unrestricted.shakeandbake(), stopping.entropy=smaa.entropy.ranking, cluster.size=detectCores()) {
  stopifnot(nr >= 1)
  n <- dim(meas)[3]
  constr <- list(constr=matrix(NA, nrow=0, ncol=n), dir=rep("<=", 0), rhs=rep(0, 0))

  ret <- list()

  for (i in 1:nr) {
    res <- find.cut(meas, nrPlanes, constr, error.func=error.func, sample.planes=sample.planes, stopping.entropy=stopping.entropy, cluster.size)
    message(i, ". Cut: ", res$share * 100, "% - h: ", sprintf("%.2f", res$h))
    ret[[i]] <- res
    old.nr <- nrow(constr$constr)
    constr <- eliminateRedundant(mergeConstraints(constr,
                               plane.constraint(res$point,
                                                res$normal,
                                                plane.side(tgt, res$point, res$normal))))
    message("- eliminated ", old.nr - nrow(constr$constr) + 1, " redundant constraints")
  }
  ret
}

# select columns of a matrix, ALWAYS returning a matrix
select.col <- function(m, i) {
  m[, i, drop=FALSE]
	matrix(m[, i], nrow=nrow(m))
}

# select rows of a matrix, ALWAYS returning a matrix
select.row <- function(m, i) {
  mi[i, , drop=FALSE]
}

question.entropy.pairwise <- function(w, w1, w2, cut, meas, entropy=smaa.entropy.choice, equalWProbs=FALSE) {
  sel <- partition.points(w, cut$point, cut$normal)
  p1 <- sum(sel) / nrow(w)
  p2 <- 1 - p1

  if (equalWProbs) {
      p1 <- 0.5
      p2 <- 0.5
  }
  v1 <- smaa.values(meas, w1)
  v2 <- smaa.values(meas, w2)
  r1 <- smaa.ranks(v1)
  r2 <- smaa.ranks(v2)

  h1 <- entropy(r1)
  h2 <- entropy(r2)
  
  c('h1'=h1, 'h2'=h2, 'h'=p1 * h1 + p2 * h2)
}

error.func.entropy <- function(equal.w.prob=FALSE, entropy=smaa.entropy.ranking) {
    function(cut, meas, constr, weights) {
      source('lib/code.R') # hack around bug in parallel backend
      nrW <- dim(meas)[1]
      w1 <- harSample(mergeConstraints(constr,
                                       plane.constraint(cut$point,
                                                        cut$normal, TRUE)), nrW)
      w2 <- harSample(mergeConstraints(constr,
                                       plane.constraint(cut$point,
                                                        cut$normal, FALSE)), nrW)
      question.entropy.pairwise(weights, w1, w2, cut, meas,
                                entropy=entropy, equalWProbs=equal.w.prob)
    }
  }

error.func.equisplit <- function() {
  function(cut, meas, constr, weights) {
      source('lib/partition.points.R') # Fix parallel backend bug
      sel <- partition.points(weights, cut$point, cut$normal)
      p1 <- sum(sel) / nrow(weights)
      p2 <- 1 - p1
      c('h1'=p1, 'h2'=p2, 'h'=dist(rbind(c(p1, p2), c(0.5, 0.5))))
  }
}

## Choose the best cutting plane (in terms of entropy).
##
## constr: constraints defining W'
## meas: measurements for the alternatives (matrix)
## cuts: a list, where cuts[[i]]$point and cuts[[i]]$normal give a point and
## normal vector defining a hyperplane
## error.func: error function to use
## parallel: whether to use parallel computing
## Return value: entropies and (index of the) best hyperplane
best.cutting.plane <- function(constr, meas, cuts, error.func, parallel=TRUE) {
    nrW <- dim(meas)[1]
    n <- dim(meas)[3]

    ## sample weights to use for estimating the sizes of p(W'')
    w <- harSample(constr, nrW)
    hs <- laply(cuts, error.func, meas, constr, w, .parallel=parallel, .drop=FALSE,
                .paropts=list('.packages'='hitandrun', '.export'=c('harSample')))

    colnames(hs) <- c("h1", "h2", "h")
    list(choice=which.min(hs[,"h"]), entropies=hs)
}
