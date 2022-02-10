# What the functions do ?
# The makeCacheMatrix function takes in a matrix, and outputs
# a list, whose elements are four functions (A function is also
# an object in R.). Now suppose that we have such list called 
# "M", and "M" is acquired by " M <- makeCacheMatrix(my_matrix)".
# Then, M$get() will retrieve "my_matrix", and M$get_inverse() will
# retrieve its inversed partner (If we haven't set its value
# using M$set_inverse(), the result will be NULL.). M$set(another_matrix) 
# will change the environment where the matrix is now a different one and its
# inversed partner is reset to NULL. This is where the lexical scoping
# rule and superassignment come into play. The superassignment "<<-"
# operator will not only set the value of "x" in the current environment, 
# but will also set all variables named "x" in its series of parent 
# environments all the way to the global environment (if no variable 
# named "x" is found along the way to the global environment, then R will
# create a global "x" in addition to the local one.). This is why after we
# call M$set(another_matrix), we will get exactly the new matrix
# by M$get(). The M$set() and M$get() were all defined in the same environment
# when makeCathMatrix(my_matrix) is called (We can see, for example, that their 
# environment is all like 0x0000022259a78140 in the console). Therefore, 
# if the M$set changes the environment of 0x0000022259a78140 (by 
# superassignment operator), then M$get will be operated under 0x0000022259a78140
# with new variable-value pairs, producing the new matrix we expect it to be.

makeCacheMatrix <- function(x = matrix()) {
    my_inv <- NULL
    set <- function(y){
        x <<- y
        my_inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inv) my_inv <<- inv
    get_inverse <- function() my_inv
    list(set = set, get = get, 
         set_inverse = set_inverse, 
         get_inverse = get_inverse)
}


# What the functions do ?
# "x$get_inverse" will retrieve the object named "get_inverse", which
# is just the corresponding function object. Then we will check
# if the inverse matrix has been set or not. If it is there, then
# we return cacheSolve with that existing inverse matrix. If not, we
# use x$get() to retrieve the original matrix "my_matrix", to compute its 
# inversed partner and then to set this result into our original special 
# matrix "M". If we call M$get_inverse() at this point, we will notice that 
# the result, no longer NULL, is exactly its inversed partner now. This is also 
# achieved by the underlying mechanism of lexical scoping as mentioned earlier.

cacheSolve <- function(x, ...) {
    my_inv <- x$get_inverse()
    if(!is.null(my_inv)){
        message("getting cached data")
        return(my_inv)
    }
    my_matrix <- x$get()
    my_inv <- solve(my_matrix, ...)
    x$set_inverse(my_inv)
    my_inv
        ## Return a matrix that is the inverse of 'x'
}
