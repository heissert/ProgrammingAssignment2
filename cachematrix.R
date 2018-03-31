#This function creates a special "matrix" object that can cache its inverse.

#step 1
#define getters and setters for both data objects i.e. matrix x and inverse i
makeCacheMatrix <- function(x = matrix()) { #necessary to set default
  i <- NULL #necessary to set default
  set <- function(y) { #set function for x 
    x <<- y #object from parent environment
    i <<- NULL #clear previously cached inverse
  }
  get <- function() x #get function for x 
  setinverse <- function(inverse) i <<- inverse #set function for inverse i # defined in parent environment
  getinverse <- function() i #get function for inverse
  list(set = set,#return list
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#step 2
#2.  `cacheSolve`: This function computes the inverse of the special "matrix" returned  by `makeCacheMatrix` above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
