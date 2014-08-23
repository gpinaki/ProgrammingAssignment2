## Caching the Inverse of a matrix
## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
		}
	get <- function() x 
	setinv <- function(solve) m <<- solve
	getinv <- function() m
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv )
	}

## cacheSolve does the matrix inverse

cacheSolve <- function (x = matrix(), ...) {
	m <- x$getinv()
	if(!is.null(m)) {
			message("getting cached data")
			return(m)
			}
	matrix <- x$get()
	m <- solve(matrix, ...) ## This step actually inverse the matrix
	x$setinv(m)
	m
	}
