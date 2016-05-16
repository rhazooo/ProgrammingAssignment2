nm  <- function(n){
    m <- n^2
    print(m)
}

makeCacheMatrix2 <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    get_inv <-function() inv
    set_inv <-function(inver) inv<<- inver  
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
    
}


## Write a short comment describing this function

## the following method uses the object created via the first function to store and retrieve matrix inverses
cacheSolve2 <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$get_inv()
    if(!is.null(inv)){
        return(inv)
    }
    data <-x$get()
    inv <- solve(data)
    x$set_inv(inv)
    inv
}