## "makeCacheMatrix" creates a function that loads an inverted square dataset
 ## into a matrix. cacheSolve" loads the inverted matrix or if it is "Null"
## creates the inverted matrix itself. The combination of these two functions
## increases the processing speed of inverting a large data set. The two
## functions work in combination with each other.


makeCacheMatrix <- function(x = matrix()) {

        cache <- NULL
        #creates an empty undefined matrix

        set <- function(y) {
                x <<- y
                cache <<- NULL
        }

        get <- function() x

        setMatrix <- function(inverse) cache <<- inverse
        # inverts the matrix and stores in "cache"

        getInverse <- function() cache
        # gets the inverted matrix from cache

        list(set = set, get = get, setMatrix = setMatrix, getInverse = 	getInverse)
        # returns the created functions to the working environment
        # builds the matrix from the called data set
}

cacheSolve <- function(x, ...) {
        ## attempts to get the inverse of the matrix stored in cache
        cache <- x$getInverse()

        # returns inverted matrix from cache if it exists
        # else creates the matrix
        if (!is.null(cache)) {
                message("getting cached data")

                # display matrix in console
                return(cache)
        }

        # creates matrix if it does not exist
        matrix <- x$get()

        # verify matrix is square and invertible
        # if not, process exceptions
        tryCatch( {
                # set and return inverse of matrix
                cache <- solve(matrix, ...)
        },
        error = function(e) {
                message("Error:")
                message(e)

                return(NA)
        },
        warning = function(e) {
                message("Warning:")
                message(e)

                return(NA)
        },
        finally = {
                # set inverted matrix in cache
                x$setMatrix(cache)
        } )

        # display matrix in console
        return (cache)
}
