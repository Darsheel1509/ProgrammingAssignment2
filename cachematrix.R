

makeCacheMatrix <- function(x = matrix()) {
  c <- NULL
  set <- function(y) {
    x <<- y
    c <<- NULL  ## ASSIGNING VALUE OUTSIDE THE ENVIRONMENT
  }
  get <- function() x 
  setting_inverse <- function(inverse) c <<- inverse     
  getting_inverse <- function() c
  list(set = set, get = get,
       setting_inverse = setting_inverse,
       getting_inverse = getting_inverse)      

}



cacheSolve <- function(x, ...) {
  c <- x$getting_inverse()
  if(!is.null(c)) {
    message("getting the cached data") ##CHECKING IF THE INVERSE ALREADY BEEN CALCULATED
    return(c)
  }
  data <- x$get()
  c <- solve(data, ...)  ## INVERSE BEING CALCULATED USING SOLVE
  x$set(c)
  c
}
