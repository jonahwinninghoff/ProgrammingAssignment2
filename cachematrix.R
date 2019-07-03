## The function 'makeCacheMatrix' develops, so as to assign
## a matrix complimentary with another function 'cacheSolve'
## that makes this matrix invertible.

## The function 'makeCacheMatrix' formulates using similar outline
## that function 'makeVector' uses but for caching matrix.

# At first, you have to build up matrix using the function 'rbind' before
# applying the function 'makeCacheMatrix.'

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse)
    m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## The function 'makeCacheMatrix' formulates using similar outline
## that the 'cachemean' uses but for caching invertible matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
## Return a matrix that is the inverse of 'x.'

## Demonstrate a procedure using both functions
## 1.) Form the matrix and save it in 'x'
##        rbind(c(1, -2), c(3, -1)) -> x
##
## 2.) Put 'x' in the () of makeCacheMatrix and save it in 'm'
##      > makeCacheMatrix(x) -> m
##
##      > m$get()
##              [,1] [,2]
##        [1,]    1   -2
##        [2,]    3   -1
##
## 3.) Put 'm' in the () of cacheSolve and the inverse matrix depicts
##
##      > cacheSolve(m)
##             [,1] [,2]
##        [1,] -0.2  0.4
##        [2,] -0.6  0.2

