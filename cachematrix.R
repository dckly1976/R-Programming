## This script store matrix in a environment with its inverted value
## so, this way, you just need to calculate the inverted matrix one time
## and obtain this info from cache.

## makeCacheMatrix is the first function, it store the original matrix
## and create functions to get it, modify and assign and inverted matrix
makeCacheMatrix <- function(x = matrix()) {
  inverted_mtx = NULL
  # subfunction if it needed to modify stored matrix
  set_new_matrix <- function(new_matrix){
    x <<- new_matrix
    inverted_mtx <<- NULL
  }
  # subfunction to get original matrix
  get <- function() x
  # subfunction to assing a value to inverted matrix
  set_inverted <- function(inverted) inverted_mtx <<- inverted
  # subfunction to get only inverted matrix, using original matrix as key
  get_inverted <- function() inverted_mtx
  list(set_inverted = set_inverted,
       get = get, get_inverted = get_inverted)
}

## cacheSolve is a function that will retrieve matrixes stored in cache, or, 
## if its first time, will compute inverted matrix and store in cache for
## further uses
cacheSolve <- function(x, ...) {
  # first, we check if original matrix (x) already as a value for inverted on cache
  ## to that, we use get_inverse() that is assign to x object (original matrix)
  get_inv_mtx = x$get_inverted()
  ## if has an value, retrun inverted matrix
  if(!is.null(get_inv_mtx)) {
    message("getting cached inverted matrix")
    return(get_inv_mtx)
  }
  # if not, compute inverted matrix for the first time and store in cache
  original_mtx = x$get()
  inverted_mtx = solve(original_mtx, ...)
  x$set_inverted(inverted_mtx)
  inverted_mtx
  ## Return a matrix that is the inverse of 'x'
}

## tests
A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)

# creating stores for matrix a, inside b variable
b = makeCacheMatrix(A)
# get matrix (equals to A)
b$get()
# get inverse matrix before have it (must be NULL)
b$get_inverted()
# Caching inversed matrix for B (1st time, so must calculate)
cacheSolve(b)
# matrix remains the same
b$get()
# inversed matrix now as a value
b$get_inverted()
# getting inversed matrix from cache
cacheSolve(b)
