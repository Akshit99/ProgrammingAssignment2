## Put comments here that give an overall description of what your
## functions do
# These functions compute the inverse of the matrix and return the inverse 
# if already computed from cache
## Write a short comment describing this function
#This function creates a special "matrix" object that will cache its inverse
 makeCacheMatrix <- function(x = matrix()) {
    n <- NULL     #initialize inverse with NULL value
       set <- function(z){      #set function for new matrix
         x <<- z
         n <<- NULL
       }
      get <- function() x    # to get matrix x
      setinverse <- function(inverse) {   #assign inverse value in parent
        n <<- inverse 
      }
      getinverse <- function(){ n}  # get inverse where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
     # for $ operator
  }

## Write a short comment describing this function
# this function calculates inverse of a matrix
#if already computed then returned from cache
#if not computed then its set to new inverse

 cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
        n <- x$getinverse
          if( !is.null(n) ) {
           message("getting cached data")
            return(n)
          }
          d <- x$get()  ## Get the matrix from our object
          n <- solve(d) %*% d  ## Calculate the inverse using solve function
          x$setinverse(n)  ## Set the inverse to the object
           n       ## Return the matrix
 }
 
