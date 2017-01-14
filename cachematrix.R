## Below are pair of functions to cache the inverse of a matrix 
## which saves computation Time 


##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(mat = matrix()) {
      invmat <- NULL #  initialization of Inverse Matrix 
  
      #setter function to Initialize when required
        setmatrix <- function(y) {
            mat <<- y
            invmat <<- NULL
        }
    
     #getter function to retrieve data of Matrix 
         getmatrix <- function() mat
  
     #setter function to cache the inverse of Matrix using Lexical Scoping
         setinverse <- function(inverse) invmat <<- inverse
  
     #getter function to retrieve inverse of Matrix
         getinverse <- function() invmat
  
     #list to call the functions by name
         list(setmatrix = setmatrix, getmatrix = getmatrix,
              setinverse = setinverse,
              getinverse = getinverse)
}


## This function computes the inverse of special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        invmat <- x$getinverse() #retrive the Inverse Martix 
  
     #condition to evaluate the inverse Matrix
        if(is.null(invmat)) {
             message("computing matrix inverse")
        }else{
             message("getting cached matrix inverse")
             return(invmat)
        }
  
     #Compute the Inverse of a Matrix
        data <- x$getmatrix()
        invmat <- solve(data)
        x$setinverse(invmat)
        invmat
}



