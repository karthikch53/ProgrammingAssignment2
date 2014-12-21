## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object 'm' that can cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Check if inverse is already present.
## If yes, return it from cache.
## Else, compute inverse, store it in cache and then return.

## Call using b<-cacheSolve(makeCacheMatrix(a))
## where is a is any square matrix
## c <- a%*%b should return an Identity matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)){
          print("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setInv(inv)
        inv  
}
