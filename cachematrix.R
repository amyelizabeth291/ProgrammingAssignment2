## When used in conjuction these functions can solve matricies and store their
## inverses so they do not need to be solved each time the inverse is used

## Make a matrix that can be solved using the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() {x}
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## Solve the matrix if it has not yet been solved, or return cached data if the
## matrix has been solved before. 

cacheSolve <- function(x) {
        i <- x$getinverse()
        if(!is.null(i)){
                message ("getting cached data")
                return (i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
