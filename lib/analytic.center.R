library(plyr)
library(hitandrun)

## Transforms the constraint direction to the ROI type (i.e. '=' -> '==')
dir.to.roi <- function(dir) {
    dir[dir == '='] = '=='
    dir
}

# filter a set of constraints
filterConstraints <- function(constr, sel) {
  list(constr = constr[['constr']][sel, , drop=FALSE],
       rhs = constr[['rhs']][sel],
       dir = constr[['dir']][sel])
}

eq.constr <- function(constr) {
  filterConstraints(constr, constr[['dir']] == '=')
}

iq.constr <- function(constr) {
  x <- filterConstraints(constr, constr[['dir']] != '=')
  stopifnot(all(x[['dir']] == '<='))
  x
}

analytic.center <- function(constraints) {
    n <- ncol(constraints$constr)
    transform.inv <- simplex.createTransform(n=n, inverse=TRUE,
                                             keepHomogeneous=FALSE)
    transform = simplex.createTransform(n=n, inverse=FALSE,
        keepHomogeneous=FALSE)
    
    constr.nminus.one <- transformConstraints(transform, constraints)
    
    ineq <- iq.constr(constr.nminus.one)
    ineq$rhs <- ineq$rhs - ineq$constr[,n]
    ineq$constr <- ineq$constr[,-n]

    pt <- createSeedPoint(constr.nminus.one, randomize=TRUE, homogeneous=TRUE)[-n]
    m <- nrow(ineq$constr)

    optim.func <- function(x) {
        -sum(laply(1:m, function(i) {
            log(ineq$rhs[i] - ineq$constr[i,] %*% x)
        }))
    }

    res <- constrOptim(pt, optim.func, method='Nelder-Mead',
                       ui=-ineq$constr, ci=-ineq$rhs)
    
    stopifnot(res$convergence == 0) ## should converge
    transform %*% c(res$par, 1)
}
