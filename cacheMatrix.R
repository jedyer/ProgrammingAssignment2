makeCacheMatrix <- function(x = matrix()) {  #create function with null matrix
  m <- NULL   #initialize matrix to null
  set <- function(y) {
    x <<- y   #assign input to object x in the parent environment
    m <<- NULL  #assign NULL to the m object in the parent environment
  }
  get <- function() x  #function for the inverse matrix function
  setinverse <- function(solve) m <<- solve  #inverse of a matrix
  getinverse <- function() m  #getter for solve m
  list(set = set, get = get,  #objects to pass to function cacheSolve
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {  #get cache of the matrix
  m <- x$getinverse()
  if(!is.null(m)) {   #if inverse already exists, retreive it
    message("getting cached data")
    return(m)
  }
  data <- x$get()  #if inverse does not exist, calculate inverse
  m <- solve(data, ...)
  x$setinverse(m)
  m
}