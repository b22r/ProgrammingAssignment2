#creates list of set and get functions to get inverse matrix or calculate inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  #set m to null
  m <- NULL
  
  #set x variable passed in function (parent variable) equal to y and set m to null to indicate set has been called
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # return value of x
  get <- function() {
    x
  }
  
  # calculate the matrix inverse and store in m
  setinverse <- function(solve) {
    m <<- solve
  }
  
  # return m, which is either the matrix inverse if setinverse waslast called or null if set was last called
  getinverse <- function() {
    m
  }
  
  # returns list of functions to call
  list (set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

#returns the cached inverse matrix if already calculated; if not calculate the inverse
cacheSolve <- function(x, ...) {
  # set m equal to the value of getinverse - this is the inverse of the matrix if calculated (cached)
  m <- x$getinverse()
  # if matrix inverse has already been calculated and thus isn't null, return the cached inverse and exits function
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # set data equal to the matrix if prior section hasn't already returned the value (meaning it's not cached if it gets to here)
  data <- x$get()
  # calculate the matrix inverse
  m <- solve(data, ...)
  
  #  set x equal to inverse 
  x$setinverse(m)
  
  #return inverse
  m
  
}