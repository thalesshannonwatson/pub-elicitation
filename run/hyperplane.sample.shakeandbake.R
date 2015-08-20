library(hitandrun)
library(MASS)
library(plyr)

### ... are the extra params to pass to sab.init
do.shakeandbake.sampling <- function(constr, N, corner.inds=NULL, x0=NULL, ...) {
    n <- ncol(constr$constr)
    k <- if (is.null(corner.inds)) n else n - ncol(corner.inds)
    stopifnot(k <= n && k > 1) # sanity check
    stopifnot(is.null(corner.inds) || nrow(corner.inds)==N)

    num.faces <- nrow(constr$constr)-1
    num.corners <- if (is.null(corner.inds)) 0 else ncol(corner.inds)
    num.pts.needed <- n - 1 - num.corners

    state <- sab.init(constr, eliminate=FALSE, x0=x0, ...)
    
    transform.inv <- simplex.createTransform(n=n, inverse=TRUE,
                                             keepHomogeneous=FALSE)
    transform <- simplex.createTransform(n=n, inverse=FALSE, keepHomogeneous=FALSE)

    final.samples <- matrix(nrow=0, ncol=n)
    total.pts.needed <- (n - 1) * N
    while (nrow(final.samples) < total.pts.needed) {
        to.sample <- (total.pts.needed - nrow(final.samples)) * (n-1-num.corners) / (n-1)

        result <- sab.run(state=state, n.samples=to.sample)
        state <- result$state

        if (num.corners == 0) {
            sample <- result$samples
        } else {
            sample <- add.corners(result$samples, corner.inds, num.pts.needed)
        }

        face.ind.sample <- face.indices(sample, constr)

        planes.ok <- as.vector(check.pts(face.ind.sample, n-1))
        pts.ok <- as.vector(t(matrix(rep(planes.ok, n-1), ncol=n-1))) # R is so kool
        
        ## get the samples out
        final.samples <- rbind(final.samples, sample[pts.ok,])
        if (num.corners > 0) {
            corner.inds <- corner.inds[!planes.ok,,drop=FALSE]
        }
    }
    stopifnot(nrow(final.samples) == N * (n-1)) # keep the sanity

    planes <- alply(matrix(1:nrow(final.samples), nrow=(n-1)), 2, function(inds) {
        all.pts <- final.samples[inds,]
        pts.minus.1d <- t(transform.inv %*% t(cbind(all.pts, 1)))

        list(point=as.matrix(all.pts[1,]),
             normal=transform %*% c(t(points.to.plane(pts.minus.1d)), 0))
    })
    list(xn=final.samples[nrow(final.samples),], planes=planes)
}

add.corners <- function(sample, corner.inds, pts.per.sample) {
    nr.samples <- nrow(sample) / pts.per.sample
    stopifnot(nrow(corner.inds) == nr.samples)

    n <- ncol(sample)

    matrix.inds <- matrix(1:nrow(sample), nrow=pts.per.sample)
    res <- alply(1:nr.samples, 1, function(ind) {
        corners <- aaply(corner.inds[ind,], 1, function(x) {
            arr <- array(0, dim=n)
            arr[x] <- 1
            arr
        })
        rbind(sample[matrix.inds[,ind],], corners)
    })
    res <- do.call(rbind, res)
    dimnames(res) <- c()
    res
}

## Checks according to faces, which point-sets all lie on a single plane
check.pts <- function(faces, num.per.face) {
    aaply(matrix(1:nrow(faces), nrow=num.per.face), 2, function(inds) {
        all(colSums(faces[inds,]) < num.per.face)
    })
}

## Sample separating hyperplanes such that reference alternatives differ on
## at most k attributes
## @return a function(constr, N) which has inputs
##   constr: HAR linear constraint structure
##   N: the amount of planes to sample
##   and that returns a list of N planes (each with $point and $normal)
##   ... are the extra params to pass to sab.init
sample.planes.restricted.shakeandbake <- function(k, ...) {
    function(constr, N, x0=NULL) {
        n <- ncol(constr$constr)
        stopifnot(k < n)
        constr <- eliminateRedundant(mergeConstraints(simplexConstraints(n),
                                                      constr))

        ## Sample the n-k attributes along which the alternatives are equal
        D <- sapply(1:N, function(i) { sample.int(n, n-k) })
        if (is.vector(D)) { # In case n-k=1
            D <- as.matrix(D)
        } else {
            D <- t(D)
        }
        do.shakeandbake.sampling(constr, N, corner.inds=D, x0=x0, ...)
    }
}

### ... are the extra params to pass to sab.init
sample.planes.unrestricted.shakeandbake <- function(...) {
    function(constr, N, x0=NULL) {
        n <- ncol(constr$constr)
        constr <- eliminateRedundant(mergeConstraints(simplexConstraints(n),
                                                      constr))
        do.shakeandbake.sampling(constr, N, x0=x0, ...)
    }
}

## Gives the normal of the plane with the given points
##
## pts: n x n matrix of points for the normal
## PRECOND: dim(pts)[1] == dim(pts)[2]
## @return normal of the plane span by the pts
points.to.plane <- function(pts) {
    stopifnot(dim(pts)[1] == dim(pts)[2]) # precond
    n.comps <- aaply(pts[-1,,drop=FALSE], 1, '-', pts[1,])
    if (is.matrix(n.comps)) {
        n.comps <- t(n.comps)
    } else { ## 1 point m as matrix -> column vector. pfft sometimes I hate R
        n.comps <- as.matrix(n.comps)
    }
    Null(n.comps)
}

## Give indices of the faces of the samples
##
## samples: p x n matrix (p n-dim samples)
## faces: HAR linear constraint structure for the k faces
## PRECOND: ncol(samples) == ncol(faces$constr)
## @return a matrix with p rows containing face indices of the samples
face.indices <- function(samples, faces, EPS=1E-15) {
    stopifnot(ncol(samples) == ncol(faces$constr)) # precond
    inds <- aaply(samples, 1, function(x) {
        ## sanity check - should be the constraint said below
        stopifnot(faces$constr[1,] == 1)
        ## first row is the x_1 + ... + x_n = 1, so drop that out
        vals <- sapply(2:nrow(faces$constr), function(y.ind) {
            x %*% faces$constr[y.ind,]
        })
        ## Samples can belong to more than 1 face for being very close to
        ## vertices
        abs(vals  - faces$rhs[-1]) <= EPS
    }, .drop=FALSE)
    as.matrix(inds)
}
