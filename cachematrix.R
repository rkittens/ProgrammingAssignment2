## The functions work in tandem with each other to help solve the problem at hand
## In this instance that problem is "what is the inverse of the matrix?"
## The first function sets up caching functions to speed up future calls
## The second function actually solves the equation but references the first function to set cached values
## If the second function has cached values it doesnt do anything but return the value if it doesnt it solves instead

## The variable i is declared as NULL to establish it in the environment.
## Then four functions are declared for setting and getting the data as well as setting the inverse and getting the inverse
## If the set function is called the i variable is set back to null while the data is set
## The get returns the data to be evaluated or set to a variable and then evaluated
## The setInverse function grabs the inverse and caches the value for the getInverse function to return at a later time
## In the final line a list with names is returned and then transformed to a matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    as.matrix(list(set = set, get = get, setInverse = setInverse,getInverse = getInverse))
}


## The cacheSolve function tries to pull from cached values
## If it succeeds it will grab the variable i and end the function
## If it is returned a NULL value it proceeds to use the nested functions from above
## It will first grab the data and then use the solve function on the data and assign it to i
## To help speed up the process in the future the function will use setInverse to cache i
## After that it returns i as the value

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
