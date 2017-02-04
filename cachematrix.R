#My functions create the ability to retrieve a previously-calculated matrix inverse
#instead of having to re-calculate every time you want it.

#This function makes a matrix object that can cache its inverse

makeCacheMatrix = function(x = matrix())
{
  inv = NULL
  set = function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinverse = function(solveinverse) inv <<- solveinverse
  getinverse = function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#This function either calculates the inverse of the matrix object, or if it has 
#already been run once so there's a cached value, then it retrieves the 
#cached data instead

cacheSolve = function(x,...)
{
  inv = x$getinverse()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  data = x$get()
  inv = solve(data, ...)
  x$setinverse(inv)
  inv
}
