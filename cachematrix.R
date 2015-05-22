## These two functions allow the ability to compute and cache the inverse of a matrix

## makeCacheMatrix defines a matrix object. It has matrix and inverse attributes,
## with getters and setters for each

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                     # Instantiate matrix objects without inverse
        set <- function(y) {            # Setter function for matrix attribute
                x <<- y
                inv <<- NULL            # Setting the matrix resets the inverse (to null)
        }
        get <- function() x             # Getter function for matrix attribute
        setinv <- function(usrinv) inv <<- usrinv       # Setter function for inverse attribute
        getinv <- function() inv                        # Getter function for inverse attribut
        list(set = set, get = get,      # List the functions when an object is instantiated
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve returns the inverse of a matrix either from memory if it is cached, 
## or by computing it if necessary

cacheSolve <- function(x, ...) {
        inv <- x$getinv()               # Get the inverse attribute from the matrix object using the getter
        if(!is.null(inv)) {                     # If the inverse attribute exists, indicate that the inverse
                message("getting cached data")  # came from the cache, prepare to return it
                returnme <- inv
        }
        else{                           # If the inverse attribute was null, get the matrix, compute the inverse, 
        matrix <- x$get()               # set the inverse attribute, and prepare to return the inverse
        inv <- solve(matrix)
        x$setinv(inv)
        returnme <- inv
        }
        
        returnme                                       ## Return the inverse
}

