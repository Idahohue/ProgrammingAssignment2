
				### R_WEEK 3_PROGRAMING ASSIGNMENT:2 ###

## ---------------------
## ASSIGNMENT OBJECTIVE: 
## ---------------------
## TO WRITE A PAIR OF FUNCTIONS THAT CACHE THE INVERSE OF A MATRIX.

## ----------------
## ASSUMPTIONS:
## ----------------
## 1.THE MATRIX SUPPLIED IS ALWAYS INVERTIBLE


## ----------------
## OVERALL SUMMARY:
## ----------------
## USING TWO FUNCTIONS FOR CACHING THE INVERSE OF A MATRIX; WHICH DO THE FOLLOWING:

## FUNCTION 1: makeCacheMatrix:
## THIS FUNCTION CREATES A SPECIAL "MATRIX" OBJECT THAT CAN CACHE IT'S INVERSE.

## FUNCTION 2: cacheSolve: 
## THIS FUNCTION COMPUTES THE INVERSE OF THE SPECIAL "MATRIX" OBJECT RETURNED
## BY makeCacheMatrix FUNCTION ABOVE. IF THE INVERSE HAS ALREADY BEEN CALCULATED
## (AND THE MATRIX HAS NOT CHANGED), THEN THE FUNCTION cacheSolve SHOULD RETRIEVE
## THE INVERSE FROM THE CACHE.


## ---------------------
## FUNCTION DESCRIPTION:
## ---------------------
## makeCacheMatrix:

## THE FOLLOWING FUNCTION CREATES A SPECIAL "MATRIX" OBJECT THAT CAN CACHE
## ITS INVERSE, IT CONTAINS A FUNCTION TO:
## 1. SET THE VALUE OF THE "MATRIX" OBJECT
## 2. GET THE VALUE OF THE "MATRIX" OBJECT
## 3. SET THE VALUE OF IT'S INVERSE
## 4. GET THE VALUE OF IT'S INVERSE

makeCacheMatrix <- function(x = matrix()) {
	inverse_mat <- NULL
      set <- function(y) {
                x <<- y
                inverse_mat <<- NULL
      }
      get <- function() x
      setMatrixInverse <- function(solve) inverse_mat <<- solve
      getMatrixInverse <- function() inverse_mat
      list(set = set, get = get,setMatrixInverse = setMatrixInverse, getMatrixInverse = getMatrixInverse)
}



## ---------------------
## FUNCTION DESCRIPTION:
## ---------------------
## cacheSolve:

## THE FOLLOWING FUNCTION CALCULATES THE INVERSE OF A MATRIX OF THE SPECIAL "MATRIX"
## OBJECT CREATED WITH THE ABOVE FUNCTION (makeCacheMatrix). HOWEVER, IT FIRST CHECKS
## TO SEE IF THE INVERSE OF THE MATRIX HAS ALREADY BEEN CALCULATED (AND THE MATRIX
## HAS NOT CHANGED). IF SO, IT GETS THE INVERSE OF THE MATRIX FROM THE CACHE AND SKIPS
## THE COMPUTATION. OTHERWISE, IT CALCULATES THE INVERSE OF THE MATRIX AND SETS IT'S 
## VALUE IN THE CACHE VIA THE setMatrixInverse FUNCTION.


cacheSolve <- function(x, ...) {
        
	inverse_mat <- x$getMatrixInverse()
      if(!is.null(inverse_mat) & x == y) {
		message("Inverse of the matrix already exists; getting the cached data")
            return(inverse_mat)
      }
      data_newMatrix <- x$get()
      inverse_mat <- solve(data_newMatrix, ...)
      x$setMatrixInverse(inverse_mat)
      inverse_mat
}



