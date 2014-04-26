makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
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
#---------------------------------------------------------
cachemean <- function(x, ...) {
  m <- x$getmean()           #query the x vector's cache         
  if(!is.null(m)) {           #if there is a cache
    message("getting cached data") 
    return(m)                #just return the cache, no computation needed
  }
  data <- x$get()             #if there's no cache
  m <- mean(data, ...)        #we actually compute them here
  x$setmean(m)                #save the result back to x's cache
  m                           #return the result
}
#------------------------------------------------------------
a <- makeVector(c(1,2,3))
a
class(a)
class(a$get)
a$get
a$get()
a$set(c(4,5,6))
a$get()
#----------------------------------------------------------
b <- cachemean(a)
b
b <- cachemean(a)
