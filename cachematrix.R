## makeCacheMatrix is used to create a matrix-like object with a cache-capability, 
## cacheSolve uses this object to invert a matrix.

## makeCacheMatrix: the input matrix A is used to create a matrix-like object, say Atilde, with four function-like features:
## 1. Atilde.set will set A to the matrix of our choosing and also sets the cache to NULL
## 2. Atilde.get will return A
## 3. Atilde.setinv will set the cache to our choosing
## 4. Atilde.getinv will return the cache
## The variable cache is stored in system memory! The output is a list of four functions determined by the input A.
makeCacheMatrix <- function(A=matrix()) {
  cache  <- NULL
  set    <- function(B) {
    A     <<- B
    cache <<- NULL
  }
  get    <- function() A
  setinv <- function(B) cache <<- B
  getinv <- function() cache
  list(set = set, get = get, 
       setinv = setinv, getinv = getinv)
}

## cacheSolve: the input has to be an object A created by makeCacheMatrix.
## It uses the the four functions available to compute the inverse of A,
## most importantly if A has been inverted already, it is stored in cache,
## and in this case a message is printed indicating that the inverse is
## being obtained from memory. If it is not computed earlier, i.e. cache==NULL,
## then we obtain a local matrix that was the input of makeCacheMatrix originally,
## invert it with solve(), and then store to the cache.
cacheSolve <- function(A) {
  Ai <- A$getinv()
  if(!is.null(Ai)) {
    message("getting cached inverse")
    return(Ai)
  }
  Alocal <- A$get()
  Ai     <- solve(Alocal)
  A$setinv(Ai)
  Ai
}
