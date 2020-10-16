library(MASS)
makeCacheMatrix <- function(x=matrix() ){
    inv<-NULL      #initializing inverse as NULl
    set<-function(y){
                      x<<-y
                      inv<<-NULL
                        }
get<-function()x                        ##function to get matrix x
setinv<-function(inverse)  inv<<-inverse
getinv<-function(){
                       inver<-ginv(x)
                       inver%*%x         ##function to get inverse of the matrix
                    }
list(set = set,get = get,setinv = setinv,getinv = getinv)

}

cacheSolve <- function(x, ...)
{
   inv<-x$getinv()
   if(!is.null(inv)){
                         message("getting cached data!")
                         return(inv)
}
data<-x$get()
inv<-solve(data, ...)
x$setinv(inv)
inv                          ##Return a matrix that is the inverse of 'x'
}