## Sample separating hyperplanes in unrestricted fashion
## by taking n points in different faces of the polytope.
## This is achieved by taking 2x the required samples to
## be sure we have sufficient amount of samples. It's not
## optimal but anyway the shake and bake sampling is not the
## bottleneck in our implementation.
##
## @return a function(constr, N) which has inputs
##   constr: HAR linear constraint structure
##   N: the amount of planes to sample
##   and that returns a list of N planes (each with $point and $normal)
sample.planes.unrestricted.shakeandbake.oversample <- function() {
    function(constr, N) {
        n <- ncol(constr$constr)
        constr <- eliminateRedundant(mergeConstraints(simplexConstraints(n), constr))
        num.faces <- nrow(constr$constr)-1

        n.faces <- n-1
        state <- har.init(constr, boundary=TRUE)

        transform.inv <- simplex.createTransform(n=n, inverse=TRUE, keepHomogeneous=FALSE)
        transform = simplex.createTransform(n=n, inverse=FALSE, keepHomogeneous=FALSE)
        ## sample 2x the amount of points times amount
        ## needed for the cuts times the amount of constraints
        ## to have a chance of sampling a sufficient amount
        n.points.to.sample <- 2 * N * n.faces
        samples <- har.run(state=state, n.samples=n.points.to.sample)
        inds <- face.indices(samples$samples, constr)
        stopifnot(nrow(inds) == nrow(samples$samples) && ncol(inds) >= 1) # sanity check
        ## Do the checks in n-1 dims
        samples.minus.1d <- t(transform.inv %*% t(cbind(samples$samples, 1)))
        partitions <- partition.samples(samples.minus.1d, inds, num.faces)
        
        partition.sizes <- sapply(partitions, nrow)
        sampled.face.inds <- replicate(N, sample(1:num.faces, n.faces, prob=partition.sizes))
        if (!is.matrix(sampled.face.inds)) {
            sampled.face.inds <- t(sampled.face.inds)
        }
        ## FIXME: should check first that there's enough samples in each partition
        planes.minus.1d <- sample.planes.from.partitions(partitions, sampled.face.inds)

        lapply(planes.minus.1d, function(p) {
            list(point = transform %*% c(p$point, 1),
                 normal = transform %*% c(p$normal, 1))
        })
    }
}

## Requires the face.inds to be a matrix with each column one sample of face indices
sample.planes.from.partitions <- function(partitions, face.inds, corners=NULL) {
    apply(face.inds, 2, function(inds) {
        points <- matrix(nrow=0, ncol=ncol(partitions[[max(inds)]]))
        for (j in inds) {
            part.j <- partitions[[j]]
            if (!is.matrix(part.j)) {
                part.j <- t(as.matrix(part.j))
            }
            if (nrow(part.j) < 1) {
                stop('partition with no points, cannot sample')
            }
            pt.ind <- sample(nrow(part.j), 1)
            pt <- part.j[pt.ind,]
            partitions[[j]] = part.j[-pt.ind,]
            stopifnot(is.vector(pt) && length(pt) > 0) # sanity check
            points <- rbind(points, pt)
        }
        if (!is.null(corners)) {
            points <- rbind(points, corners)
        }
        list(point=points[1,], normal=t(points.to.plane(points)))
    })
}

partition.samples <- function(samples, inds, nmax) {
    stopifnot(nrow(samples) == nrow(inds)) # precond
    res <- list()
    for (i in 1:nmax) {
        mt <- samples[inds[,i]==TRUE,]
        if (length(mt) == 0) {
            mt <- matrix(nrow=0, ncol=0)
        } else if (!is.matrix(mt)) {
            mt <- t(as.matrix(mt))
        }
        res[[i]] <- mt
    }
    res
}
