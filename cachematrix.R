
## Matrix calc package can be used
## command is.singular.matrix()function checks if the matrix passed is invertible
## The can be used to result is TRUE or FALSE.

## This function sets and gets the passed matrix and its inverse.
## it will handle declaration of local variables and free variables 
makeCacheMatrix <- function(A = matrix()) {
      
  B<-matrix()  
  set <- function(a) {
    A <<- a   ## Non-singular invertible matrix
    B <<- NULL
  }
  get <- function() A
  setinverse <- function(invmatrix) B <<- invmatrix
  getinverse <- function() B
  as.matrix(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
    
}


## This function will first check if the inverse Matrix is available in B
## it will check if B is the inverse of A
## i.e matrix A multiplied wit its Inverse=identity matrix
## if ok then return value of B
## else get matrix A and use "solve" to get inverse of A and pass it to B
## and then set the value of B .

cacheSolve <- function(A, ...) {
        ## Return a matrix that is the inverse of 'x'
  B<- A$getinverse()
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
     B <- solve(data)
     A$setinverse(B)
     B
   } ##end of else
  } ##end of if

  
}  ## end of Cachesolve
