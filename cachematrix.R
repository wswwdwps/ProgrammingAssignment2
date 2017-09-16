# makeCacheMatrix will read in a matrix and produce a special list which contains 4 functions.
# cacheSolve will read in the special list and calculate matrix inversion. 
# cacheSolve will first try to get inversion from cache. If fail, it will calculate one and cache it.

makeCacheMatrix <- function(m = matrix()){
	# input: matrix
	# output: list(set, get, setInv, getInv)
    inversion <- NULL
    set <- function(x){
        m <<- x
        inversion <<- NULL
    }
    get <- function() m
    setInv <- function(inv) inversion <<- inv
    getInv <- function() inversion
    list(set = set, get = get, setInv = setInv, getInv = getInv)  # return
}

cacheSolve <- function(m, ...){
	# input: list made by makeCacheMatrix
	# output: get cached matrix inversion, otherwise calculate one and cache it
    inv<-m$getInv()
    if(!is.null(inv)){
        message("getting cached inversion...")
        return(inv)
    }
    else{
        mat<-m$get()
        result<-solve(mat, ...)
        m$setInv(result)
        result  # return
    }
}