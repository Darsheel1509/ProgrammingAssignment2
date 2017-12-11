

makeCacheMatrix <- function(x = matrix()) {
  c <- NULL
  set <- function(y) {
    x <<- y
    c <<- NULL
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
    message("getting the cached data")
    return(c)
  }
  data <- x$get()
  c <- solve(data, ...)
  x$set(c)
  c
}
