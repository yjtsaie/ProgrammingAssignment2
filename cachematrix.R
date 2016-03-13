## Find the inverse of a special matrix by looking into inverse cache matrix first
## if it exists, use, itif not solve the inverse and put back to the cache matrix
##
##  Test script is at the bottom of the program code


## function: makeCacheMatrix:
## create list object which provides methods to cache and access a matrix and it's inverse
## program codes
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


## Return a matrix that is the inverse of 'x'
##looking into cache first, if not there then compute it 

cacheSolve <- function(x, ...) {
  
  m <- x$getinv()
  if (!is.null(m)) {
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

## test run script as following:
##
## > test1 <- matrix(1:4, 2,2)
## > test2 <- matrix(1:9, 3,3)
## > test3 <- matrix(1:6, 2,3)
## > test4 <- matrix(2:10, 3,3)
## > test1
##     [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > test2
##     [,1] [,2] [,3]
## [1,]    1    4    7
## [2,]    2    5    8
## [3,]    3    6    9
## > test3
##     [,1] [,2] [,3]
## [1,]    1    3    5
## [2,]    2    4    6
## > test4
##     [,1] [,2] [,3]
## [1,]    1    2    5
## [2,]    1    1    1
## [3,]    1    4    3
## > a <-makeCacheMatrix()
## > b <-makeCacheMatrix()
## > c <-makeCacheMatrix()
## > d <-makeCacheMatrix()
## > a$set(test1)
## > b$set(test2)
## > c$set(test3)
## > d$set(test4)
## > a$get()
##     [,1] [,2]     : show matrix already in cache
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(a)   : compute inv and put it in cache
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5 
##
## > CacheSolve(a) : compute inv by getting from cache
## > cacheSolve(a)
## getting cached data
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > cacheSolve(b)   : error test
## Solving Inverse Error:             : my program message
##  Lapack routine dgesv: system is exactly singular: U[3,3] = 0NULL : R error message
## 
##  > cacheSolve(c) : error non square metrix
##  Solving Inverse Error:            : my program message
##  'a' (2 x 3) must be squareNULL    :: R error message
##
## > cacheSolve(d)
##     [,1] [,2] [,3]
## [1,] -0.1  1.4 -0.3
## [2,] -0.2 -0.2  0.4
## [3,]  0.3 -0.2 -0.1
##
## > cacheSolve(d) : compute inv by getting from cache
## getting cached data
## [,1] [,2] [,3]
## [1,] -0.1  1.4 -0.3
## [2,] -0.2 -0.2  0.4
## [3,]  0.3 -0.2 -0.1
##
##> b$getinv()
## NULL
## > c$getinv()
## NULL
##


