## The makeCacheMatrix develops as a tool, so as
## to assign a matrix complimentary with cacheSolve 
## that makes this matrix invertible.

## The makeCacheMatrix formulates using similar outline
## that makeVector uses but for developing matrix purpose.

# At first, you have to build up matrix by rbind before
# using makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The makeCacheMatrix formulates using similar outline
## that cachemean uses but for invertible matrix purpose.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inv)
  inv
}
## Return a matrix that is the inverse of 'x.'
