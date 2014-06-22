
#
## This function creates a special "matrix" object that can cache its inverse.
## If the original matrix is singular then the inverse is set to NULL

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  ## if determinant of x is zero than the matrix is singular
  if(det(x)==0)
    {
   setinverse <-NULL}
  else
  {setinverse <- function(solve) i <<- solve}
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache. verse.
## If the original matrix is singular then a message is written with that information

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  z<-det(data)
  if (z==0)
  {print ("the matrix is singular")}
  else
    
  {i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
 
  i <- solve(data)
  x$setinverse(i)
 i
  }  
}

