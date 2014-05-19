## solve function takes long time to calculate so it's a good idea to cache result
## there is no need to calculate each time the same matrix. cached value is presented
##
## Two functions were created:
## 1. makeCacheMatrix 
## 2. cacheSolve

## Usage examples:
# 0. i <- 1:3
# 1. matr <- 1 / outer(i - 1, i, "+")
# 2. result <- makeCacheMatrix(matr)
# 3. > result$get()
#             [,1]      [,2]      [,3]
#   [1,] 1.0000000 0.5000000 0.3333333
#   [2,] 0.5000000 0.3333333 0.2500000
#   [3,] 0.3333333 0.2500000 0.2000000
# 4. > cacheSolve(result)
#        [,1] [,2] [,3]
#   [1,]    9  -36   30
#   [2,]  -36  192 -180
#   [3,]   30 -180  180
# 5. > cacheSolve(result)
#   getting cached data
#        [,1] [,2] [,3]
#   [1,]    9  -36   30
#   [2,]  -36  192 -180
#   [3,]   30 -180  180

## stores and retrieves initial and solved matrices
makeCacheMatrix <- function(x = matrix()) {
    # initialization of variable
    matrix <- NULL

    ## set function
    # stores variables x (as y value) and matrix (as NULL)
    set <- function(y) {
        x <<- y
        matrix <<- NULL
    }
    
    ## get function
    # returns x value
    get <- function() x
    
    ## setmatrix function
    # assigns variable to matrix
    setmatrix <- function(variable) matrix <<- variable
        
    ## getmatrix function
    # returns solved matrix
    getmatrix <- function() matrix
    
    ## name the list members
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## tries to invert matrix. 
## if it has cached value, returns it.
## if it doesn't have cached value, calculates it, stores it in cache and returns it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # check value in cache
    matrix <- x$getmatrix()
    if(!is.null(matrix)) {
        # found cache, return it
        message("getting cached data")
        return( matrix )
    }
    # no cache found, let's get matrix
    data <- x$get()
    # solve it
    matrix <- solve(data, ...)
    # cache it
    x$setmatrix(matrix)
    # return it
    matrix
}
