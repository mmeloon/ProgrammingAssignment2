## These two functions work together to compute the matrix inverse of a
## given matrix. Because this calculation can be computationally expensive,
## we cache inverses that we compute so we don't need to recompute them.

## I am using the naming style recommended by Hadley Wickham in
## Chapter 5 of his book "Advanced R", namely using four (4) spaces
## for indentation and using underscores to separate words in variable
## name rather than camelCase or some other convention. I have left
## the names makeCacheMatrix and cacheSolve alone since they were specified
## in the stubs given to us.

## Function makeCacheMatrix returns a list containing a function to
## 1) Set the value of the matrix
## 2) Get the value of the matrix
## 3) Set the value of the matrix inverse
## 4) Get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- matrix()
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    set_inv <- function(solve) inv <<- solve
    get_inv <- function() inv
    list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## Function cacheSolve computes the matrix inverse of a specified matrix x.
## It first checks to see if the inverse has already been computed. If so
## it simply returns the cached matrix inverse. If not then it calculates
## the inverse of the matrix and sets the value in the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inv()
    if(!is.null(inv) && x == x$get()) {
        ## If the inverse has already been calculated (and the matrix has
        ## not changed), then retrieve inverse from the cache.
        message("getting cached data")
        return inv
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inv(inv)
    inv
}
