makeCacheMatrix <- function(x = matrix()){
  m <- NULL                       #Creates special matrix;setting m to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
}
  get <- function() x
  setinverse <- function(inverse) m <<- inverse #Caches matrix w/ set & get in m
  getinverse <- function() m 
  list(set=set,
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}

cacheSolve <- function(x=matrix(), ...){
  m <- x$getinverse()                      #Setting m to retrieve cached data
  if(!is.null(m)){
    message("Getting cached data")  
    return(m)
  }
  data <- x$get()              #Obtaining cached data
  m <- solve(data, ...)       #Obtaining inverse of cached data
  x$setinverse(m)             
  return(m)                  #Returning inverse matrix
}
 # To test this code out, generate a random matrix e.g 
 # my_matrix <- matrix(c(4,6,3,1),2,1)
 # Then my_matrix2 <- makeCacheMatrix(my_matrix)
 # Finally cacheSolve(my_matrix2)

