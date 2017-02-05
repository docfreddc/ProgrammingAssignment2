## These functions find the inverse of matrices and saves their values
## If the inverse of a matrice has already been found, it gets the value
## from the Cache instead of recalculating it again, to save time

## Creates a list containing the inverse matrix, that can be reviewed
## to avoid having to calculate it again

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Looks to see if the inverse of a matrix has been cached
## If so, doesn't calculate it again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
