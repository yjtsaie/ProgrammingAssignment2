## Find the inverse of a special matrix by looking into inverse cache matrix first
## if it exists, use, itif not solve the inverse and put back to the cache matrix

## create matrix object can cache a matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
      m<- NULL
      set <- function(y) {
            x<<-y
            m<<-NULL
      }
      get <- function() x
      setinv <- function(inv) m <<- inv
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
      
}


##   ## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  m<- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data_m <- x$get()
 
  tryCatch(   {
                m <- solve(data_m, ...)
              },
              error = function(err) {
                message("Solving Inverse Error:")
                message(err)
                return(NA)
              },
              Warning = function(err) {
                message("warning:")
                message(err)
                return(NA)
              },
              finally = {
                x$setinv(m)
              }
  )
  
  return(m)

}
