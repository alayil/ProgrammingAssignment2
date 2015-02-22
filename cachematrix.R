## https://github.com/alayil/ProgrammingAssignment2
## R Programming Assignment 2
## Lexical Scoping--caching the inverse of a matrix
## Developed By: Praveen Syamakomalan 
## Feb 22, 2015
## The makeCacheMatrix function creates a matrix, which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
i <- NULL
sett <- function(y) {
x <<- y
i <<- NULL
}
get <- function() x
setinverse <- function(inverse) i <<- inverse
getinverse <- function() i
list(
sett = sett,
gett = gett,
setinverse = setinverse,
getinverse = getinverse)
}

## The cacheSolve function calculates the inverse of the special "matrix"
## the matrix which created with the makeCacheMatrix function.
## It first checks if the inverse is already calculated.
## Then it gets the inverse from the cache and skips the computation, if it exists.
## Else, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
i <- x$getinverse()
if(!is.null(i)) {
message("Procuring cached data")
return(i)
}
data <- x$get()
i <- solve(data, ...)
x$setinverse(i)
i
}