## makeCacheMatrix() contains set(), get(),setcolMeans() and getcolMeans() 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y= matrix()) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setcolMeans <- function(colMeans) m <<- colMeans
    getcolMeans <- function() m
    list(set = set, get = get,
         setcolMeans = setcolMeans,
         getcolMeans = getcolMeans)
}

## computes the inverse of the special "matrix" returned by makeCacheMatrix 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getcolMeans()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- colMeans(data, ...)
    x$setcolMeans(m)
    m
}
################################################
## test funcion makeCacheMatrix():
################################################
#### 1:test makeCacheMatrix() and get matrix from cache
####   input a matrix matrix(1:6, 3, 2)
## a <- makeCacheMatrix(matrix(1:6, 3, 2))
## a$get()

#### 2:set an assignment matrix and get the matrix by set()
## a$set(matrix(1:20, , 5))
## a$get()

#### 3:set an assignment colMeans matrix, and get the matrix by setcolMeans()
## a$setcolMeans(matrix(1:10, , 5))
## a$getcolMeans()

################################################
## test funcion cacheSolve()
################################################
#### computes the inverse of the special "matrix" returned by makeCacheMatrix 
## cacheSolve (a)

