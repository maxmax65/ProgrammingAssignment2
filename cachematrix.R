## These two functions are used to create and set a special kind of square matrix that contains also
## its inverse, calculated once and stored in the special_matrix  and then fetched from it for further usage 

## This function creates a special square matrix that can contain its inverse
## It creates a list containing the functions to:
## 1. set the values of the matrix
## 2. get the values of the matrix
## 3. set the inverse matrix
## 4. get the inverse matrix

## Once created a special matrix M, it is possible to set its values calling the
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
	## the list with the four functions to handle the special matrix is returned
    list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function calculates or fetches from the special_matrix  (created with the above function) received
## as argument its inverse matrix
## It verifies first if the inverse matrix has already been calculated and is already cached in the
## special matrix received as argument

cacheSolve <- function(x, ...) {
    ## this function returns a matrix that is the inverse of 'x'
    inverse<-x$getinverse()
	## if the inverse matrix is already present in the "special matrix", it is fetched and returned
    if(!is.null(inverse)){
		message("getting cached inverse matrix")
		return(inverse)
	}
	##  retrieve the matrix from special_matrix x with get () function
    matrice<-x$get()
	## apply solve () function to produce the inverse of "matrice"
    inversa<-solve(matrice)
    ## store the inverse matrix in the special_matrix x with setinverse () function
	x$setinverse(inversa)
	## return the inversa matrix 
    inversa
}


## Example:
## >B<-makeCacheMatrix() ## creates an object B of "special matrix" kind
## >B$set(matrix(c(2,1,1,2),2,2)) ## sets the values of special_matrix B
## >cacheSolve(B) ## calculates the inverse matrix of the B's values and stores it in B itself
## >cacheSolve (B) ## retrieves the inverse matrix already cached in B
## >B$get()%*%B$getinverse() ## returns a unitary square matrix