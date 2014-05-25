
## Matrix calc package can be used
## this function will check if the matrix passed is invertible

makeCacheMatrix <- function(A = matrix()) {
    ## The command is.singular.matrix()result is TRUE or FALSE.
    ## if is.singular.matrix()=FALSE 
  
  B<-matrix()  
  set <- function(a) {
    A <<- a   ## Non-singular invertible matrix
    B <<- NULL
  }
  get <- function() A
  setinverse <- function(invmatrix) B <<- invmatrix
  getinverse <- function() B
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  B<- x$getinverse()
  A<- get()
  if (!is.matrix(B)=NULL)
  {
    if (B%*%A = identity matrix (ones on the diagonal)  )
    {
      message("getting cached data")
      return(B)
    }
  else
   {
     data <- A$get()
     B <- solve(A)
     A$setinverse(B)
     B
   } ##end of else
  } ##end of if

  
}  ## end of Cachesolve
