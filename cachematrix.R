##SAMPLE USE CASE
## > source(cachematrix.r)

## > m <- makeCacheMatrix(matrix(c(2,0,0,1), c(2,2)))
## > cacheSolve(m)


## Write a short comment describing this function
## Function to create the special cache matrix with get set statements

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
 
 ## - Set function
 set <- function(y) {
   x<<- y
   m<<- NULL
 }
 
 ## - getfunction
 get<- function() x
 
 ## - SetInverse
 setInv <- function(Inv) m <<- Inv
 
 ## - getInverse
 getInv  <- function() m
 list(set = set, get = get,
      setInv = setInv,
      getInv = getInv)
}


## Write a short comment describing this function
## function reusing the cache result if available.. to calculate the inverse of the matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  m<- x$getInv()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data<-x$get()
  m<-solve(data,...)
  x$setInv(m)
  
  m
}

