## the makeCacheMatrix stores information regarding the solution to the inverse of
## a matrix in the environment defined within the function. The cacheSolve function
## checks the cached data for a solution to the inverse.  If one exists, it prints it
## If one does not exist, it reads the matrix in makeCacheMatrix, solves for its
## inverse, and stores it in the makeCacheMatrix environment

## This function creates a list creating the following cached information 
## regarding the solution to an inverse of a matrix within an environment 
## defined in the function: 1) the matrix being solved for, 2) the inverse of
## the matrix (if it has already been aquired)

makeCacheMatrix <- function(x = matrix()) { 
        my_inverse<-NULL ##sets up the variable to store the inverse matrix to NULL
        set<-function(y){
                x<<-y ## when this function is executed it changes the data in
                ## the original function to y
                my_inverse<<-NULL ## resets my_inverse (defined in the parent)
                ## environement to NULL -- very important, otherwise, a previously
                ## defined inverse may remain in the parent environment of the
                ## function. If this happens, you will get the wrong answer if your data
                ## changes!
        }
        get<-function()x ## object get is created; in it will be the data to be
        ## reviewed (the matrix)
        set_inverse<-function(solve)my_inverse<<-solve ## allows for setting up 
        ## an inverse in the my_inverse object established in the parent environment
        ## (the makeCacheMatrix environment)
        get_inverse<-function()my_inverse ## stores the inverse established in the
        ## my_inverse object
        list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
}

## This function checks the list created in the makeCacheMatrix function to see
## if it contains an inverse matrix.  If it does, it prints the inverse.
## if the object that stores the inverse matrix is 'NULL', the function reads the 
## matrix stored in the makeCacheMatrix environemnt, solves for its inverse,
## stores the inverse in the makeCacheMatrix environment, and prints it to the screen

cacheSolve <- function(x, ...) {
        my_inverse<-x$get_inverse() ## reads the object defined in the list
        ## established in the makeCacheMatrix function that holds an the inverse
        ## of the matrix
        if(!is.null(my_inverse)){
                message("getting cached data")
                return(my_inverse) ## if there an inverse matrix has been stored
                ## in the get_inverse object (that is, it is not NULL), it is printed
                ## and the function ends.
        }
        data<-x$get() ## if there is no inverse defined (get_inverse = NULL), the
        ## matrix to be analyzed is stored as the variable "data"
        my_inverse<-solve(data,...) ## the operation that creates the inverse of
        ## data and stores it as my_inverse
        x$set_inverse(my_inverse) ## the my_inverse matrix is cached within the
        ## makeCacheMatrix environment
        my_inverse
        ## Return a matrix that is the inverse of 'x'
}
