# Check if creating a cacheMatrix and
# getting its content works fine
x <- matrix(rnorm(1000000), nrow = 1000)
m <- makeCacheMatrix(x)

stopifnot(identical(m$get(), x))

# Check if cacheSolve gives the
# same result for the same matrix
# and if it truly uses caching
print(system.time(y  <- cacheSolve(m)))
print(system.time(y1 <- cacheSolve(m)))

stopifnot(identical(y, y1))

# Check if cacheSolve gives the same result
# as solve
z <- solve(x)

stopifnot(identical(y, z))

# Check if updating the matrix with
# set works correctly
x1 <- matrix(rnorm(100), nrow = 10)
m$set(x1)

stopifnot(identical(m$get(), x1))

# Check if the cache is unvalidated
# after a set()
y <- cacheSolve(m)
z <- solve(x1)

stopifnot(identical(y, z))