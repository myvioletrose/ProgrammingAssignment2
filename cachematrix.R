## the first function is to get and set up a matrix and store its Inverse (cached Inverse)
## the second is to solve the matrix and check and see whether an Inverse is already calculated; 
# if not, then it will just calculate and set up an Inverse and store it within the first function

# the makeCacheMatrix function, it is similar to the makeVector, i.e. it will return a vector or a list with 4 functions
        # set the value of the matrix
        # get the value of the matrix
        # set the inverse of the matrix
        # get the cached inverse of the matrix
        # i.e. list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
             
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y 
                m <<- NULL 
        } #we can set the matrix here 
        get <- function() x #we can get the matrix here, i.e. mCM <- makeCacheMatrix(x); mCM$get() #return x
        setInverse <- function(Inverse) m <<- Inverse #we can set Inverse here
        getInverse <- function() m #likewise, we can get Inverse here
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


# the cacheSolve function, essentially, it will return the inverse of matrix supplied in above function, e.g. cacheSolve(mCM) #using above example
# the function would first check whether an Inverse has already been calculated. 
# If so, it will return the cached Inverse. 
# Otherwise, it will calculate the Inverse, i.e. m <- Solve(data, ...) and subsequently set the values, i.e. x$setInverse(m)

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

## we can test it here
# x <- matrix(c(3,3.5,3.2,3.6), 2,2); x
# mCM <- makeCacheMatrix(x); mCM$get()
# cacheSolve(mCM)
