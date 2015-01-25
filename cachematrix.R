## Sample 25 random numbers out of 1 to 100. Mat 25 is a 5 by 5 matrix. 

sample25 <- sample(1:100, 25, replace=T)
mat25 <- matrix(sample25, nrow = 5, ncol = 5)

## [Put comments here that describe what your functions do]

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL               ## Set cached inverse matrix to NULL
        set <- function(y) {    
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)      
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()               ## Get the inverse matrix
        if(!is.null(m)) {               ## If inverse matrix not empty, return cached data
                message("getting cached data")   
                return(m)
        }
        data <- x$get()                 ## If inverse matrix empty, get matrix data
        m <- solve(data, ...)           ## Calculate inverse matrix
        x$setsolve(m)                   ## And store the inverse matrix
        m
}


## Use below code to create new data
v<-makeCacheMatrix(mat25)
cacheSolve(v)

## Use below code to get cached data
cacheSolve(v)