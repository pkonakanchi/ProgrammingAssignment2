## Assignment: Caching the Inverse of a Matrix
#Write the following functions:
        
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
        #1. set the value of the matrix
        #2. get the value of the matrix
        #3. set the value of the matrix
        #4. get the value of the matrix
makeCacheMatrix <- function(x = matrix())
{
        m <- NULL
        set <- function(y)
        {
                x <<- y
                m <<- NULL
                
        }
        get <- function() x
        setmatrix <- function(matrix)
        {
                m <- matrix
        }
        getmatrix <- function() m
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}
# Function 2 calculates the inverse of the special "matrix" created with the above function
#1 Check if inverse has already been caliculated
#2 If yes, gets the matrix from the cache and skips computation
#3 If no, caliculates the inverse of the matrix and sets the value of the 
# inverse in cache using setmean function
cacheSolve <- function(x, ...)
{
        m <- x$getmatrix()
        if(is.null(m))
        {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
