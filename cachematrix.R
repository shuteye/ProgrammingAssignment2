## Programming Assignment #2 : 
## The following functions perform matrix inverstion with the added benefit of 
## caching previously performed inversions in order to save CPU time

## The first function takes a matrix and creates a series of functions
## for the purpose of caching and retrieving invertered matrices
##

makeCacheMatrix <- function(x = matrix()) {
    mcm<-NULL
    set<- function(y) {
      x<<-y
      mcm<<-NULL
    }
    get<-function() x
    setinv<-function(inv) mcm<<-inv
    getinv<-function() mcm
    
    val<-list(set=set, get=get, setinv=setinv, getinv=getinv)
    val$set(x)
    val
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mcm<-x$getinv()
    if (!is.null(mcm)) {
      message("retrieving matrix from cache")
      return(mcm)
    }
    
    data<-x$get() #grabs x value if not cached
    mcm<-solve(data)
    x$setinv(mcm)
    mcm
    
}

# test
b<-matrix(1:4,2)
solve(b)
a<-makeCacheMatrix(b)
cacheSolve(a)
d<-makeCacheMatrix(matrix(1:4,2))
cacheSolve(d)