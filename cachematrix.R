## Put comments here that give an overall description of what your
## functions do

# Description of functions 

# The script consists of two functions, que first one which covers a list 
# of anonymous functions that will be called to solve the inverse matrix and 
# save the result to cache. 

# The second function will employ an if clause in order to evaluate if the 
# matrix was been solved before, and if it is the case it will return the 
# value stored in cache. 


## Write a short comment describing this function


# Function makeCacheMatrix(): Lists of anonymous functions that will solve the
# matrix and will save the result in cache. 



makeCacheMatrix <- function(x = matrix()) {
  n <- NULL

# Function set(): set the value of y to the value of x in cache.
  set <- function(y) {
    x <<- y
    n <<- NULL
  }

# Function get(): retrieves the value of x
  get <- function() x

# Function setinverse(): sets the value of the resolved inverse
# matrix to the value of n in cache. 
  setinverse <- function(inverse) n <<- inverse

# Function getinverse(): retrieves the value of n
  getinverse <- function() n

# list of functions assigned 
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

# Function cacheSolve(): It evaluates if the value for the calculation
# of the inverse matrix is in cache and, if it is so, returns it. 

cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'
  n <- x$getinverse()

# Conditional to determine if the is data of the calculation in cache
  if (!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  
# When there is no value in cache it resolves the matrix 
  data <- x$get()
  n <- solve(data, ...)
  
# Sets the value of the inverse matrix to cache for future
# calling
  x$setinverse(n)
  n
}

#test#

m<-matrix(c(1,2,3,4),2,2)
# First time displays the message that it calculates the inverse
# matrix. Second time it displays the cache valued. 
m1<-makeCacheMatrix(m)
cacheSolve(m1)


t<-matrix(1:6,2,2)
# First time displays the message that it calculates the inverse
# matrix. Second time it displays the cache valued. 
t1<-makeCacheMatrix(t)
cacheSolve(t1)

d<-matrix(1:8,2,2)
# First time displays the message that it calculates the inverse
# matrix. Second time it displays the cache valued. 
d1<-makeCacheMatrix(d)
cacheSolve(d1)

