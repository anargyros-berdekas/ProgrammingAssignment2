## Coursera Course: "R Programming"
## Programming Assignment for Week 3
## Name: Anargyros Berdekas
## Email: aberdekas1993@gmail.com
## July 2017, Rotterdam, The Netherlands

## The objective of this R script is to
## determine the inverse of a matrix

## The first function ("makeCacheMatrix") 
## makes the matrix

## The second function ("cacheSolve") uses the 
## first function in order to 
## determine the inverse matrix
## and offers the inverse matrix as output

## FIRST FUNCTION: I define the name and 
## argument of function to set and get the 
## value of matrix
## This function gets a matrix as input
## and sets the value of the variable to 
## be used then by the second function

## ------- VERY IMPORTANT NOTE !!! -------
## The names of variables and structure
## of this script were inspired by the example 
## script offered on the instructions for 
## Progr. Asn. 2
## These instructions of the assignment
## are available e.g. on the following URL 
## https://github.com/anargyros-berdekas/ProgrammingAssignment2/blob/master/README.md
## -----------------------------------------

makeCacheMatrix <- function(x = matrix()) {
  
  ## Next: Create and set inverse_var to NULL 
  inverse_var <- NULL 
  
  ## Next: Create the set function to set value of matrix
  set <- function(y)
  {                     
    ## Next: we set the value of the matrix
    x <<- y
    
    ## Next: for new matrix, we set inverse_var back to null
    inverse_var <<- NULL 
  }
  
  ## Next: Create the get function to get value of matrix
  get <- function() x
  
  ## Next: Create the set function
  setinverse <- function(inverse) inverse_var <<- inverse
  
  ## Next: Create the getinverse function
  getinverse <- function() inverse_var 
  
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}

## NEXT: The second function ...
## I will define the name (cacheSolve) 
## and arguments of 
## function to do matrix inverse
## This function uses the previous function
## named makeCacheMatrix
## in order to yield the inverse matrix
## Also, this function show a small message
## during the operation to the user

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse_var <- x$getinverse()
  
  ## Next: if inverse_var NOT null, get data and show message
  if(!is.null(inverse_var)) {
    message("getting cached data")
    return(inverse_var)
  }
  
  ## Next: get matrix and put it in "data"	
  data <- x$get()
  
  ## Next: sove our objective of doing inverse of matrix
  inverse_var <- solve(data, ...)
  
  ## Next: function output is our inverse matrix
  x$setinverse(inverse_var)
  inverse_var
}