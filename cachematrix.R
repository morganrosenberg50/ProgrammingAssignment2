## 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
      
      mInv <- NULL
      set <- function(y){
            x <<- y
            mInv = NULL
      }
      get <- function () x
      
      list(set=set, get = get)
      
      setInverse <- function(inv) mInv <<- inv
      getInverse <- function () mInv
      
      list(set=set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function (x,...){
      m <- x$getInverse()
      
      if(!is.null(m)){
            message ("getting cached data")
            return (m)
      }
      myData <- x$get()
      
      m <- solve(myData)
      
      x$setInverse(m)
      
      m
}
