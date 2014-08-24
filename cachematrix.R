## makeCacheMatrix makes object with matrix and caches its inverse
## cacheSolve inverts a matrix in makeCacheMatrix and writes inverse in cache

## creates list with matrix and its inverse

makeCacheMatrix<- function(x) {
     m <- NULL
     set <- function(y) {           #set matrix object
          x <<- y
          m <<- NULL
     }
     get <- function() x            #get matrix object
     setInverse <- function(Inverse) m <<- Inverse
     getInverse <- function() m
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## inverts a matrix in makeCacheMatrix object or takes inverse from cache 

cacheSolve<- function(x, ...) {
     m <- x$getInverse()
     if(!is.null(m)) {              #check for inverse matrix in cache
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)          #inverts matrix if no inverse in cache
     x$setInverse(m)
     m
}

