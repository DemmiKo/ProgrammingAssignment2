## These Functions set and get a matrix and then calculate the inverse matrix

## The function 'makeCacheMatrix'  set a matrix and then get that matrix
## Also set the inverse matrix and get the inverse matrix. 
## It not make calculations only save values to vector

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The 'cacheSolve function getinverse matrix, check if it has value and if not then 
## calculate the inverse matrix by using function 'Solve' and get the calculated value to getinverse variable.
## If the getinverse has value then print a massege, otherwise calsulate inverse matrix

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
