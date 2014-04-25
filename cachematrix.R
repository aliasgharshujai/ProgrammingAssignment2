## makeCacheMatrix is a function which takes a Matrix input and returns list of 
## four different functions.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL             #begin with setting the object inverse equal to NULL
  set <- function(y) { 
    x<<-y                     #this function could be used for setting a different Input Matrix
    inverse<<-NULL
  }
  get<- function() {
    x                         #this function simply prints the value given in input matrix
  }
  setinverse <- function(matrixinverse) {
    inverse <<- matrixinverse #this function assigns the precalculated inverse of the input matrix to the object inverse
  }
  getinverse <- function() {
    inverse                   #this function simply prints the precalculated inverse of the input matrix
  }
  list(set = set, get = get,
       setinverse = setinverse, #the output of function makeCacheMartix is a list of the four functions as shown
       getinverse = getinverse)
}

## cacheSolve is a function that takes matrix input from the makeCacheMatrix function and
## returns the inverse of the input matrix. If the same input matrix has been submitted
## previously, it would return the inverse from the cache and no computations would be done.
## Otherwise, it would compute the inverse of the input Matrix provided that it is Square and it's
## determinant is non-zero.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse() #first assign cached inverse to the object inverse
  if(!is.null(inverse)) {   #if the inverse is cached
    message("Getting Cached Data") 
    return(inverse)         #no computation required as inverse already exists in cache
  }
  InputMatrix <- x$get()         #assign the input matrix to the object InputMatrix
  #check for multiple if else conditions on the InputMatrix
  if(nrow(InputMatrix)!=ncol(InputMatrix)){
    message("Please Input a Square Matrix") #return this message if InputMatrix is not square
  }
  else if(det(InputMatrix) == 0) {
    message("Determinant is Zero. Input Matrix is not Invertible") #return this message if determinant of InputMatrix is zero
  }
  else {
    inverse <- solve(InputMatrix, ...) #Use the solve function in R to calculate the inverse of InputMatrix
    x$setinverse(inverse) #Assign the inverse of InputMatrix to the Cache
    inverse  
  }  
}
