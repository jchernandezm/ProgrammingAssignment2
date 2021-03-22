## This functions create a cache  matrix with its inverse in order to optimize the overall 
## processing time and computng effort

## this function creates a mtrix wit the atribute inv, which is the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
            }
      
      get <- function() x
      setInv <- function(inversa) inv <<- inversa
      getInv <- function() inv
      
      list(set=set, get = get, setInv = setInv, getInv = getInv)
}


## this function creates updates the cace memory of a given matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      act <- y$get()
      
      if (is.null(y$getInv)){
            inv <- solve(x)
            y$setInv(inv)
      }
      
      else if ( identical(act,x)){
            
            inv <- y$getInv()
      }
      
      else{
            inv <- solve(x)
            y$setInv(inv)
      }
      
      return(inv)
}


