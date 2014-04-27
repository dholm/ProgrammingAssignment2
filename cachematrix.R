#' A package supplying matrix closures with the purpose of caching their
#' inverse.
#'
#' @name cacheMatrix-package
#' @docType package

#' Create a cacheable matrix closure.
#'
#' This function creates a closure around a matrix with the ability to cache the
#' matrix inverse.
#'
#' @param x Existing matrix.
#' @return A CacheMatrix closure.
#'
#' @export
#' @examples
#' xc <- makeCacheMatrix(matrix(c(1, 0, 0, 2), nrow=2, ncol=2))
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    getInverse <- function() inverse
    setInverse <- function(inverse) inverse <<- inverse
    list(set = set, get = get,
         getInverse = getInverse,
         setInverse = setInverse)
}

#' Get the solution for the specified cache matrix.
#'
#' If the solution has been cached, the cached value is returned, otherwise it
#' is calculated.
#'
#' @param x CacheMatrix to be solved (\code{\link{makeCacheMatrix}}).
#' @param ... Further arguments passed to solve.
#' @return The solution for the specified matrix.
#'
#' @export
#' @examples
#' xc <- makeCacheMatrix(matrix(c(1, 0, 0, 2), nrow=2, ncol=2))
#' cacheSolve(xc)
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if (is.null(i)) {
        i <- solve(x$get(), ...)
        x$setInverse(i)
    }
    i
}
