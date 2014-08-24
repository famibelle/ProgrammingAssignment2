## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##	set the value of the matrix
##	get the value of the matrix
##	set the value of the inverse of the matrix
##	get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,	setinverse = setinverse, getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

## Example : uncomment for testing purpose
# > source("ProgrammingAssignment2-master/cachematrix.R")
# > x = matrix(rnorm(9),3,3)
# > x <- x*t(x)
# > m <- makeCacheMatrix(x)
# > m$get()
          # [,1]        [,2]       [,3]
# [1,] 0.2366703 0.840554817 0.50672379
# [2,] 0.8405548 0.004644157 2.82529903
# [3,] 0.5067238 2.825299025 0.06691917
# > cacheSolve(m)
           # [,1]       [,2]       [,3]
# [1,] -17.012738  2.9315031  5.0566416
# [2,]   2.931503 -0.5135176 -0.5173627
# [3,]   5.056642 -0.5173627 -1.5035512
# > cacheSolve(m)
# getting cached data
           # [,1]       [,2]       [,3]
# [1,] -17.012738  2.9315031  5.0566416
# [2,]   2.931503 -0.5135176 -0.5173627
# [3,]   5.056642 -0.5173627 -1.5035512
