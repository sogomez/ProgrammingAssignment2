## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function contains elements to set and obtain the inverse matrix of the other matrix.
##The function must be relationed to an  object
## Example:   mc<-makeCacheMatrix()    
##Where mc will contain the makeCacheMatrix and its subfunctions
The original matrix must be asigned in the set subfunction
##Example x<-matrix(elements, rows, columns)
## mc.set(x)

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  ## Obtain the elements in the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  #Resolve inverse matrix
  setSolve <- function(solve) m <<- solve
  
  #In case inverse matrix was resolved weil present results
  getSolve <- function() m
  
  #Define all subfunctions be called in the object
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)  
  
}


## Write a short comment describing this function
##This function will call the object contains makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  ##In case the inverse matrix was resolved before, only will present the result 
  ##from memory an will finish the function
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##In case never was resolved, then will get the original matrix
  ##will resolve de inverse matrix and will
  ##keep the result in the memory of object than contains makeCacheMatrix
  ##and present the result
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m  
}
