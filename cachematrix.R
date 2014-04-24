#makeCacheMatrix : Base function creates functions and summrises through list . 
#It provides follwing functions
#1.Get matrix
#2.Set matrix
#3.Set inverse
#4.Get inverse not compute inverse
#set_inv : should not be exposed to cacheSolve as it updates the inverse potential problem
makeCacheMatrix <- function(x = matrix()) {
    g_inv <- NULL
    get_mat <- function () x
    set_mat <- function (y) {
       x <<- y 
       g_inv <<- NULL 
    }
    set_inv <- function (l_inv) {
        g_inv <<- l_inv
    }
    get_inv <- function() g_inv
     
    list(set_mat = set_mat , get_mat = get_mat ,
             set_inv = set_inv ,
             get_inv = get_inv)
}

#Computes the matrix inverse and store in the cache variable
#Return matrix inverse if available return cached version else computes the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ptm <- proc.time()
        l_inv <- x$get_inv()
    
        elapsed_time <- function() { 
            wt_time <- proc.time() - ptm
            print (wt_time)
        }
        if (!is.null(l_inv)) {
           message("getting the cached inverse")   
           elapsed_time ()
           return (l_inv)
        }
        input_mat <- x$get_mat()
        l_inv <- solve(input_mat,...)
        x$set_inv(l_inv)
        elapsed_time ()
        return (l_inv)
}

#TODO  : Test in progess
mat_test <- function (r=3,c=3) {
    mtx <- makeCacheMatrix(matrix(rnorm((r*c),mean=30,sd=15),nrow=r,ncol=c))
    cacheSolve (mtx)
    #check the time 
    #mtx <- makeCacheMatrix(matrix(rnorm((r*c),mean=30,sd=15),nrow=r,ncol=c))
    #cacheSolve (mtx)
}
