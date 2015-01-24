## The two functions are using the advantages of lexical scoping rules in R.
## 1st function set up a list to show the process of making a matrix,
## and 2nd function examine if the applied matrix had existed or not.
## If the results had already existed, then return the same result with a message " getting cache data"
## User first apply a matrix into 'makeCacheMatrix' function,
## and then use 'cacheSolve' to return the inverse matrix of the one applied.


## If the results had existed, cachSolve() will return the exisiting value stored in makeCacheMatrix()
## (or in other words, exacution environment of makeCacheMatrix() became the enclosing environment of cachSolve())
## If the results hadn't been set befor, R will go to the parent environment of mackCacheMatrix() to define given variables.
## That's why "<<-" was used to assign variable as NULL at the parent environment.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix<- function(x=matrix(){
        inv<- NULL
        set<- function(y){
                y<<- x
                inv<<- NULL
        }
        get<- function() x
        setinv<- function(solve) inv<<- solve
        getinv<- function() inv
        list(set=set, get=get,
             setinv=setinv,
             getinv=getinv)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve<- function(x, ...){
        inv<- x$getinv()
        if(!is.null(inv)){
                message("getting cache data")
                return(inv)
        }
        data<- x$get()
        inv<- solve(data,...)
        x$setinv(inv)
        inv
}