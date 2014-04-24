makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
        #Explicit new vector wipe out mean
        #<<- store value globally across enviroment
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


cachemean <- function(x, ...) {
        ptm <- proc.time()
        m <- x$getmean()
        
        elapsed_time <- function() { 
            wt_time <- proc.time() - ptm
            print (wt_time)
        }

        if(!is.null(m)) {
                message("getting cached data")
                elapsed_time ()
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        elapsed_time ()
        #elaspsed_time <- proc.time() - ptm
        #print (elaspsed_time)
        m
}

#f <- function(x, ...)
#{
#  dots <- list(...)                   #1
#  if(length(dots) == 0) return(NULL) 
#  cat("The arguments in ... are\n")
#  print(dots)
#  f(...)                              #2
#}
#
#f(1,2,3,"a", list("monkey"))




