## There are significant problems with this course on R. The instruction is scant and vague, as is the associated
## "book" on R programming. The material is scattered and incoherent, as if the instructor assembled it while high
## or stoned, and it is clear that little or no effort has been made to address issues of clarity that I'm sure 
## I'm not the first student to express. The instructions for this assigment were very much in line with everything 
## else for this course: vague and confusing. 
##
## Nevertheless I've taken the time to glean what the instructor probably meant to require,
## teach myself enough R (and linear algebra) to achieve the requested goals, and laid them out here in a script which
## should take only one argument: the matrix of interest, expressed either as a string or as a defined object. The 
## "template" that was provided is useless and even more confusing; it would have been better if the instructor had 
## simply left it out. It is telling that the assignment was created two years ago, and never updated.
##
## USAGE: cacheSolve(X), where X is a vector of interest.

## This is a rehashing of the makeCacheMatrix() function from the "template". As you can see, it only really needs
## one function, d, not four. The other three are window-dressing.
makeCacheMatrix <- function(x=matrix()) {
  a <- function(y) {
    bogon <- y
    bogon <<- y
  }
  b <- function() bogon + bogon
  c <- function() bogon^2
  
  # The following function, which is really the only useful one in this routine, solves the matrix inversion, and
  # stores both the matrix itself and said inversion to the Global Environment.
  d <- function(x) {
#    str(x) -- used to clarify what type of object x actually is
    
    solution <- solve(x)
    
    # Store the inversion to the Global Environment.
    StoredSolution <<- solution
    
    # Store the matrix to the global environment as "bogon". It makes sense that the cacheSolve() function should check first
    # to make sure the matrix it is solving corresponds to the solution that may be cached. This assignment is entirely
    # pointless without such a request.
    bogon <<- x
    StoredSolution
  }
  list(a=a,b=b,c=c,d=d)
}

## Defining the required makeCacheMatrix as a vector within this script saves typing and eliminates a massive
## source of confusion in the "template" script -- specifically, how exactly was the thing supposed to be invoked 
## by the user?

test2 <- makeCacheMatrix()

cacheSolve <- function(...) {
  input <- as.matrix(...)
#  message("To begin...")
  if (!exists("bogon")) {
    message("This matrix is new to us.")
    test2$d(input)
  } else if (identical(input,bogon)) {
    message("We've solved this inversion before.")
    StoredSolution
  } else {
    message("We've solved an inversion, but it's different from what we have in store.")
    test2$d(input)
  }
}
#  z$d()
