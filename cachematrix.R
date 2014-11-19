## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {   ## input x will be a matrix
    i <- NULL                                 ## i will be our 'inverse' and will be set to NULL every time makeCacheMatrix is called
    set <- function(y){            ## takes a input matrix
        x <<- y                 ## saves the input matrix
        i <- NULL               ## reset the mean to NULL
    }
    get <- function(){x}                      ## this function returns the value of the original matrix
    setinverse <- function(inverse){ i <<- inverse} 
                                              ## this is called by cacheSolve() during the first cacheSolve()
                                              ## access and it will store the value using superassignment
    
    getinverse <- function(){i}               ## return the cached value to cacheSolve()
    list(get = get, setinverse = setinverse, getinverse = getinverse )

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()            ## access the object x and gets the value of the inverse
    if(!is.null(i)){              ## if inverse were already cached 
        message("getting cached data")   ## send this message to console
        return(i)                        ## and return the mean
    }
    data <- x$get()                ## reach this code only if x$getinverse() returns NULL
    i <- solve(data)               ## calculate the inverse
    x$setinverse(i)                ## store the calculated inverse in x
    i
}
