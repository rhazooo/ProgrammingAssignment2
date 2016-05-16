## Creating a matrix and finding and storing the inverse of that matrix.

## it takes matrix as an argument and stores it. 
## It also has an additional feature of storing the store of the given matrix.

makeCacheMatrix <- function(z = matrix()) {
    inv_mat <- NULL
    
    set <- function(y){
        z <<- y
        inv_mat <<- NULL
    }
    
    get <- function() z
    
    setinverse <- function(inver) inv_mat <<- inver
    getinverse <- function() inv_mat
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
    
}

## This function creates the inverse of the given matrix.
## If the given matrix has already inverse matrix, the function won't change the matrix.
## The below function uses the objects from above matrix and calls them as per needed.

cacheSolve <- function(z, ...) {
    inv_mat <- z$getinverse()
    if(!is.null(inv_mat)) {
        message("getting cached data...")
        return(inv_mat)
    }
    data <- z$get()
    inv_mat <- solve(data)
    z$setinverse(inv_mat)
    inv_mat
}
