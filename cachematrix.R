## makeCacheMatrix function set or get both original_matrix & inversed_matrix
## cacheSolve function check if Cache Matrix has the invesed_matrix value
##                           if not make inversed_matrix, stores it, return it

## mMat stores original matrix, invMat stores inverse of the matrix
## set/setInvMat function set original_matrix/inversed_matrix relatively
## get/getInvMat function get original_matrix/inversed_matrix relatively

makeCacheMatrix <- function(x = matrix()) {
        
        mMat <- NULL
        invMat <- NULL
        
        set <- function(x) { mMat <<- x }
        get <- function() { mMat }
        
        setInvMat <- function(im) { invMat <<- im }
        getInvMat <- function() { invMat }
        
        list(set = set, get = get, 
             setInvMat = setInvMat, getInvMat = getInvMat)
}


## If matrix x has a inversed_matrix, the function returns it
## Otherwise get original_matrix, inverse it, set it to CasheMatrix, then return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invMat <- x$getInvMat()
        if(!is.null(invMat)) {
                message("This is cached data")
                return(invMat)
        }
        
        orgMat <- x$get()
        invMat <- solve(orgMat)
        x$setInvMat(invMat)
        invMat
}
