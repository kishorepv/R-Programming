##This program imolements a caching mechanism to deliver results to repeated queries
 #from the cache. If the query is not repeated, the solution is calculated anew and 
 #cached for the future.


#### makeCacheMatrix function has four functions; setMatrix, getMatrix, setInverse and getInverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <<- NULL
	
  
  # setMatrix sets the mat variable to the argument passed and sets 
  # the cached value of inverse (inv) to NULL
  setMatrix <- function (m) {
		mat <<- m
		inv <<- NULL
	}
  
	# getMatrix returns the stored matrix (mat)
	getMatrix <- function() { mat }

	# getInverse returns trhe stored matrix inverse (inv)
  getInverse <- function () { inv } 

	setInverse <- function (inverse) { inv <<- inverse }

	
	list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
	
	}





#### This function takes the return value of the makeCacheMatrix, checks whether the stored inverse (inv) is NULL.
#### If so the inverse is calculated and cached. If the stored (cached) inverse matrix (inv) is not NULL, then it is returned along with a message.

cacheSolve <- function(fun, x = NULL,  ...) {
        ## Return a matrix that is the inverse
		tmp <- matrix()
		matrix <- fun$getMatrix()
		if (is.null(x)) {

			if (is.null(fun$getInverse())) {
					if (!is.null(matrix)) {
						tmp <- solve(matrix)				
						fun$setInverse(tmp)
						return(tmp)
					}
					else {
					print ("Matrix is NULL")
					return;
					}

			}
			
			message("The cached ans: ")
			fun$getInverse()
		
		}

		else { # If optional argument x exists, this block is executed.
				
        if (identical(matrix, x)) {
				message("The cached ans: ")
				fun$getInverse()

				}


				else {
					fun$setMatrix(x)
					tmp <- solve(x)
					fun$setInverse(tmp)
					tmp
				}
		}

			
		
}
