## The two functions that inverse a matrix under proper conditions
# Comments about makecacheMatrix
# nrow != ncol- Checks if a matrix is SQUARE- IF NOT it EXITS

# Set's "m" as an initialized NULL object

# The function set takes the free variable y and assigns it in a different environment
# Similarly m is  defined in an environment different than the current one !
# m is also NULLL
# get is a function that stores x - the input
# setsolve assigns the object solve that is going to solve the matrix outside the
# current environment 
# getsolve is m within the current environment
# output is a four membered list

makeCacheMatrix <- function(x = matrix()) 
{
  if(nrow(x) != ncol(x))
  {
    message("Matrix must be SQUARE for inversion - Check Dimensions")
    return()
  }
  
  m <- NULL
  
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
 
  setsolve <- function(solve) m <<- solve
  
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## The following function calculates the inverse of a matrix created by the above function
# The function also checks if determinant is zero or not ..
# m is the output the first time 
# if the same matrix is input it get's the cached input; otherwise it computes..

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolv()
  if(!is.null(m)) 
    {
    message("getting cached data")
    return(m)
    }
  data <- x$get()
  detX <- det(data)
  if(detX == 0)
    {
    message("Determinant of Matrix equal to ZERO - CANNOT BE INVERTED")
    return()
    }
  m <- solve(data, ...)
  x$setsolve(m)
}
