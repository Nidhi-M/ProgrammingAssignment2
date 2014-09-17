
## MakeCacheMatrix with parameter matrix returns a list of  following  functions
#    1.Get matrix-gives the matrix
#    2.Set matrix-sets a matrix
#    3.Set the inverse of matrix-sets inverse of matrix
#    4.Get the inverse of matrix-gives inverse of matrix if set or else NULL

## CAchesolve function returns an inverse of matrix calculated earlier with
## the same matrix or calculates the inverse 
   

makeCacheMatrix <- function(x = matrix()) {
  ## returns a list of functions
  
 inverse<-NULL
 
 ## get the matrix
 get<-function() x
 
 ## set the matrix
 set <- function(y) {
       x <<- y
       inverse<<- NULL
      }
 
 ##set the inverse of the matrix
 setinverse<-function(inv)inverse<<-inv
 
 ##get the inverse of the matrix
 getinverse<-function()inverse
 
 ##create a list of the functions
 list( get = get,
       set = set,
             setinverse = setinverse,
             getinverse = getinverse)
}




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ##get inverse of matrix by calling getinverse from makeCacheMatrix  
  inverse<-x$getinverse()
  
  ## If the inverse is not null the execute this  i.e
  ## if the inverse was already calculated, return that
  ## inverse and come out of function
  
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  ## if not calculated then get the matrix
  data<-x$get()
  
  ## calculates using solve(matrix) that returns an inverse of matrix
  inverse<-solve(data)
  
  ## set the calculated inverse  
  x$setinverse(x)
  
  ##return inverse
  inverse
    
  
}


