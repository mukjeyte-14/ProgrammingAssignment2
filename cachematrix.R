1.	makeCacheMatrix(): creates a special “matrix” object that can cache its inverse.
	# Mathematically, aslo if det(matrix) is not zero or undefines the matrix has inverse
	# Solve() computes the inverse of the "matrix" 
2.	cacheSolve(): computes the inverse of the “matrix” returned by makeCacheMatrix(). If the inverse 
        # has already been calculated and the matrix has not changed, it’ll retrieves the inverse from the 		# cache directly.
makeCacheMatrix <- function(a = matrix()) {
        ## @a: a square an invertible matrix
        ## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ## This list is used as the input to cacheSolve()
        
        inv = NULL
        set = function(b) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                a <<- b
                inv <<- NULL
        }
        get = function() a
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(a, ...) {
        ## @a: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inv = a$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("Getting cached data....")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = a$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        a$setinv(inv)
        
        return(inv)
}
