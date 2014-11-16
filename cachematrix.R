## These two functions handle the creation of a special kind of square matrix
## that can contain its inverse and the calculation (or retrieval from cache) of its inverse

## This function creates a special square matrix that can contain its inverse
## It creates a list containing the functions to:
## 1. set the values of the matrix
## 2. get the values of the matrix
## 3. set the inverse matrix
## 4. get the inverse matrix

## Once created a special matrix M, it is posssible to set its values calling the
## M$set() function passing a <square_matrix> as argument

makeCacheMatrix <- function(x = matrix()) {
inverse<-NULL
	set<-function(y){
		x<<-y
		inverse<<-NULL
	}
	get<- function() x
	setinverse<- function(solve) inverse<<-solve
	getinverse<- function() inverse
	list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function calculates the inverse of the special matrix crated with the above function
## It verifies first if the inverse matrix has already been calculated and cached in the
## special matrix received as argument

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse<-x$getinverse()
	if(!is.null(inverse)){
		message("getting cached inverse matrix")
		return(inverse)
	}
	matrice<-x$get()
	inversa<-solve(matrice)
	x$setinverse(inversa)
	inversa
}


## Example:
## B<-makeCacheMatrix() ## creates a "special matrix" B
## B$set(matrix(c(2,1,1,2),2,2)) ## sets the values of matrix B
## cacheSolve(B) ## calculates the inverse of the B's values and stores it in B itself
## B$get()%*%B$getinverse() ## returns a unitary square matrix