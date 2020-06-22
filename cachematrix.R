## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
       
        
        ## 
        ##this function stores the matrix and its inverse in case it exists in the cache, 
        ##first an empty matrix is generated that will then take the dimensions of the assigned matrix,
        ##when there is an inverse matrix it will be stored for future calculations.
        
         m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function()
                x
        setinverse <- function(inverse)
                m <<- inverse
        getinverse <- function()
                m
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##it is verified if the inverse matrix was previously had, if the inverse matrix exists, it is printed on the screen.
        ##if the inverse matrix does not exist, 
        ##the calculation is carried out and it is stored in the cache in case it is required again
        
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}
