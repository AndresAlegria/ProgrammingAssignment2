#==============================================================================#
#------------------------ Function 1 makeCacheMatrix---------------------------#
#==============================================================================#
makeCacheMatrix <- function(x = matrix()) {
        k  <- NULL
        set  <- function(y){
                x <<- y
                k <<- NULL
        }
        get  <- function() x
        setinverse  <- function(inverse) k  <<- inverse
        getinverse  <- function() k
        list(set= set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

#==============================================================================#
#------------------------ Function 2 cacheSolve -------------------------------#
#==============================================================================#
cacheSolve <- function(x, ...) {
        k  <- x$getinverse()
        if (!is.null(k)){
                message("getting cached data")
                return(k)
        }
        data  <- x$get()
        k  <- solve(data, ...)
        x$setinverse(k)
        k
}

#==============================================================================#
#Matrix is a square matrix of order 2 x 2
#Matrix <- rbind(c(1, -1/8), c(-1/8, 1))  
#Matrix
#       [,1]   [,2]
#[1,]  1.000 -0.125
#[2,] -0.125  1.000
 
#matrix evaluada en funcion makeCacheMatrix
#solve.f <- makeCacheMatrix(Matrix)

#Evaluted function for 
#eval.f <- cacheSolve(solve.f)
#eval.f
#      [,1]  [,2]
#[1,] 1.016  .127
#[2,] .127 1.016
 
#verified that X^(-1) %*% X = I
#Ident <- eval.f%*%Matrix
#Ident
#     [,1] [,2]
#[1,]    1    0
#[2,]    0    1
