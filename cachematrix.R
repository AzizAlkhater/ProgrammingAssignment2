
### Greetings Peers

## Matrix inversion is usually a costly and time consuming  computation,
##  and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly.
##    so you can cache the result and look them up later when needed instead of computing them again.
##     The following functions can compute and cache the inverse of a matrix.



## the "makeCacheMatrix" function below creates a special "matrix" object that can cache its inverse.
##  it creates a special “matrix”, which is really a list containing a function to:

# 1 set the value of the matrix

# 2 get the value of the matrix

# 3 set the value of the inverse

# 4get the value of the inverse


 makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {
     x <<- y
     i <<- NULL
    
 }
 get <- function() x
 setinverse <- function(inverse) i <<- inverse
 getinverse <- function() i
 list(set = set,
      get = get,
      setinverse = setinverse,
      getinverse = getinverse) 
 }
     
##  The "cacheSolve" below computes the inverse of the special “matrix” returned by "makeCacheMatrix" above. 
##   If the inverse has already been calculated (and the matrix has not changed),
##    then the cachesolve should retrieve the inverse from the cache.## 
 
 cacheSolve <- function(x, ...) {
   i <- x$getinverse()
   if (!is.null(i)) {
     message("getting cached data")
     return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setinverse(i)
   i
 }   

## Select and run the functions above
## now you should see both the "cacheSolve" and the "makeCacheMatrix" listed under functions in the environment quadrant.
 
## to test the functions above
## I have created the following 3x3 matrix
 
M <- matrix(c(5,8,4,0,1,-3,0,6,-2),3,3) ##  and enter , now you can see M is listed under data in the environment quadrant.
M1 <- makeCacheMatrix(M)                ##  and enter , this returns a special matrix "M1" that can cache its inverse
                                        ##   and now you can see M1 is listed under data in the environment quadrant

cacheSolve(M1) # and enter, this returns the  inverse matrix from computation

## if you now run cacheSolve again

cacheSolve(M1) ## and enter, it will return the inverse matrix as above , but from cached data this time
               ## preceded by " getting cached data " in the Console
