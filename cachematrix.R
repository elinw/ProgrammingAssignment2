## This is a set of functions to create, cache and retrieve the invers
## of a matrix. 

## To use you need to first prime the cache with something like my_cache<-makeCacheMatrix(my_matrix)
## where my_matrix is an invertable matrix.
## Then cacheSolve(my_matrix).

## This function creates a list of functions for caching an inverted matrix
## and retrieving the cache data
## x should be an invertable matrix

makeCacheMatrix <- function(x = matrix()) {

        ## mi is going to be the inverted matrix
        ## We set it to NULL so we can check if the cached data exist
        mi <- NULL
        
        ## Allows us to set the value of the matrix directly.
        set <- function(y)
        {
                x <<- y
                ## If you are setting you need to clear out any old data.
                mi <<- NULL
        }
        ## Gets the matrix we are currently working with.
        get <- function() 
        {
                x
        }
        ## Solve() is the function to use to find the inverse. We are using the default
        ## If you provide just one matrix Solve() will default to providing the inverse.
        ## Solve can do a number of things so this is specifically setSolveInverse
        setSolveInverse<-function(solve)
        {
                mi <<- solve
        }
        getSolveInverse<-function()
        {
                mi
        }
        ## This is the list of functions returned.
        ## Reference e.g as x$getSolveInverse()
        list(
                set = set, 
                get = get, 
                setSolveInverse = setSolveInverse, 
                getSolveInverse = getSolveInverse
        )


}


## THis function will return the inversion of an invertable matrix
## (from cache if it exists).

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        mi <- x$getSolveInverse()

        print(mi)
        ## If mi is not empty, we're done.
        if (!is.null(mi))
        {
                message("getting cached data")
                
                return(mi)
        }
        
        ## Otherwise we have to do the work
        data <- x$get()

        ## Do a simple validation
        dims = dim(data)

        if (!identical(dims[1],dims[2]))
        {
                message("Your matrix is not square.")
                
                return(data)
                
        }
        
        mi <- solve(data, ...)
        
        ## Let's set it for the next time.
        x$setSolveInverse(mi)
        mi
}
