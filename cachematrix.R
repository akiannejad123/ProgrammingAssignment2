##  THis version is by Amanda Kiannejad composed on Aug 6 2016 for the Cousera
## course R Progamming 
## Aug 7, 2016

## makeCacheMatrix contructs the diminsions of the matrix 
## it sets the values and initialize the function for 
## the solve function which will calculate the inverse


makeCacheMatrix <- function(q= matrix()) {
  matInv <- NULL
  setCacheMatrix <- function(g) {  #assigns arg to q in the parent environment
    q <<- g
    matInv <<- NULL   # NULL value is an object with unseficied dims
  }
  getCacheMatrix <- function() q
  setMatrixInverse <- function(solve) matInv <<- solve 
  getMatrixInverse <- function() matInv
  list(setCacheMatrix = setCacheMatrix, getCacheMatrix = getCacheMatrix,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


cacheMatrix <- function(q, ...) {
  matInv <- q$getMatrixInverse()  
  if(!is.null(matInv)) {   
    message("getting cached matrix data")
    return(matInv)   #if the matrix is empty nothing is calculated
  }
  Mat <- q$getCacheMatrix()  #this retreives the matrix
  matInv <- solve(Mat, ...)
  q$setMatrixInverse(matInv)      #this sets the matrix 
  matInv
}