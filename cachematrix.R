## This function cache the calculation of inverse matrix 

## first function 
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

##second function calculates the inverse of the special "matrix" created with the above function.
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function

##https://stackoverflow.com/questions/24904683/caching-the-mean-of-a-vector-in-r/47723281#47723281

makeVector <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheinv <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
