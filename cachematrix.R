## Put comments here that give an overall description of what your
## functions do


## This function will cache the inverse of a square 2 x 2 matrix .  
## It includes four local functions "set" that assings 
## the matrix, "get" that keeps the matrix, "storeinverse" will  store de in-
## verse (to be calculated by the cacheSolve function below) and getinverse
## that calls for the calculation or otherwise retrieves the result


makeCacheMatrix <- function(x = matrix()){

     matriz <- NULL
        
     set <- function(y) {
           x <<- y
           matriz <<- NULL
        }
        
        get <- function(){x}
        
        storeinverse <- function(inverso){ 
                matriz <<- inverso
        }
        
        getinverse <- function(){matriz}
        
        list(set = set,
             get = get,
             storeinverse = storeinverse,
             getinverse = getinverse)
        
        
}


## Write a short comment describing this function

## This function calculates the inverse of a 2x2 matrix using solve(), before 
## doing it checks if it has been calulated before and if does it if no
##  calculation was performed before, otherwise it calls the stored (cached)
##  information and gives it back (in this case it also prints a notice 
## to let know that it is not calculating again) 


cacheSolve <- function(x, ...) {

    matriz <- x$getinverse()
                
    while (!is.null(matriz)) {
                        message("not calculating inverse, looking in cache")
                        return(matriz)
                }
                
                mi_data <- x$get()
                matriz <- solve(mi_data, ...)
                x$storeinverse(matriz)
                matriz
        
}
