##This repo has an R file that has two functions, i.e, to make a cache matrix and another to computer the inverse of the cache matrix 
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## To create a special matrix and cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        mat_ <- NULL
        set <- function(y) {
                x <<- y
                mat_ <<- NULL
        }
        get <- function() x
        setCache <- function(inverse) mat_ <<- inverse
        getCache <- function() mat_
        list(set = set, get = get,
             setCache = setCache,
             getCache = getCache)
        
}


## to compute the nverse of the matrix stored in cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat_ <- x$getCache()
        if(!is.null(mat_)) {
                message("getting cached data...")
                return(mat_)
        }
        data <- x$get()
        mat_ <- solve(data, ...)
        x$setCache(mat_)
        mat_
        
}

