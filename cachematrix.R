makeCacheMatrix <- function(x = matrix()) {
         m <- NULL 
         set <- function(y) { 
                 x <<- y  
                 m <<- NULL       ## it also re-initialises m to null 
         } 
         get <- function() x      ## return the input matrix x 
         setinverse <- function(inverse) m
         getinverse <- function() m   ## return the cached inverse of x 
         list(set = set, get = get, 
              setinverse = setinverse, 
              getinverse = getinverse) 
 } 

 
 
 
  ## cacheSolve calculates the inverse of the matrix 
  ## It first checks to see if the inverse has already been caclulated. 
  ## If exists, it retrieves the inverse from the cache 
  ## and ignores the rest of the function. Otherwise, it calculates the matrix inverse 
  ## and sets the value of the inverse in the cache m.  
  ## x$set(newmatrix) # change matrix
  ## x$get # get the setted matrix
  ## x$setInverse # set the inversed matrix
  ## x$getInverse # get the inversed matrix
 
 cacheSolve <- function(x, ...) { 
          
         m <- x$getinverse() 
         if(!is.null(m)) {    ## checks if inverse exists
                 message("retrieving cached result") 
                 return(m)    ## if exists, retrieve matrix from cache and exits function
         } 
         z <- x$get()      ## if inverse not exists, get input x as z
         m <- solve(z, ...)  ## and solves it
         x$setinverse(m)        ## and set solved results to m
         m                      ## and returns m
 } 
