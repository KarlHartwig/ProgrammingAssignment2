## This file contain two functions used to calculate the inverse of a matrix and save this
## to cache via a "setter and getter" approach.


## This function is used to create a special "CacheMatrix" object which contains
## a local solution space used to cache inverse matrices.
makeCacheMatrix <- function(x = matrix(),...) {
  
        # When a new object is created set the invese of the matrix to NULL as
        # this has not yet been calculated
        MatInv <- NULL
        
        # define "set function" used to set the original matrix x within the "Cache Matrix" object
        set <- function(y) {
          x <<- y
          MatInv <<- NULL
        }
        
        # define "get function" used to retrieve data of the original matrix x 
        get <- function() x
        
        # define function for storing the inverse of the matrix
        setInv<- function(MatInv) MatInv <<- MatInv
        
        # define function for retrieving the stored inverse of the matrix
        getInv <- function() MatInv
        
        # return the created object
        return(list(set = set, get = get, setInv = setInv, getInv = getInv))
}


## This function checks if the inverse of a matrix is stored within the "cashe matrix" object x
## If it doesn't exist the inverse is calcualted and pushed to cache.
## it is assumed that all matrices supplied are square and invertible
cacheSolve <- function(x, ...) {
        ## Retrieve the inverse of 'x' from within the "Cashe Matrix" special object
        MatInv <- x$getInv()
        
        # check if the inverse matrix exists in cashe and if so return it and exit function 
        if(!is.null(MatInv)) {
          message("getting cached data")
          return(MatInv)
        }
        
        # if matrix didnt exist; get the original data from within the "cache matrix" object
        data <- x$get()
        
        # calculate the inverse of the matrix 
        MatInv <- solve(data)
        
        # set the cashe of the inverse to the new calculated value
        x$setInv(MatInv)
        
        # return the calculated inverse to the user
        return(MatInv)
}
