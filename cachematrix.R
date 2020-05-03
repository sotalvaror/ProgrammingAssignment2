## This code was made to write a code to Cache the Inverse of a Matrix.
##For this purpose 2 functions was made: 'makeCacheMatrix' and 'cachesolve'

## The function 'makeCacheMatrix' retrieves a special "matrix" object that
##cahce its inverse

##In this assignment we are going assume that the matrix supplied is always
##invertible, so there is no code to check it.

makeCacheMatrix <- function(M = matrix()) {
    MI<-NULL
 
    
## M: matrix has to be inverted
##MI: Inverse of M
    
    set<-function(y=matrix){
        M<<-y
        MI<<-NULL

        
    } ##set a new matrix into a previously saved makeCacheMatrix object
    get<-function()M ## calls the matrix stored into a makeCahceMatrix object
    setinv<-function(inv)MI<<-inv ## set NULL MI
    getinv<-function()MI ##Calls MI
    list(set=set,get=get,setinv=setinv,getinv=getinv) 
    
    #makeCacheMatrix returns a list whit the the functions defined

}


## The function 'cachesolve' compute the inverse of the special "matrix"
##returned by 'makeCacheMatrix' above. If the inverse has alredy been
##calculated, then 'cachesolve' rerieve the inverse from cache

cacheSolve <- function(M, ...) {
    MI<-M$getinv()                      ##Retrieve MI form makeCacheMatrix
    if(!is.null(MI)){                   ##if MI its no null
        message("getting cached data")  ##gives a message
        return(MI)                      ##Return MI from makeCacheMatrix 
    }
    data<-M$get()           ##set 'data' whit the matrix M in makeCacheMatrix
    MI<-solve(data,...)     ##Assing to MI the inverted matrix of 'data' 
    M$setinv(MI)            ##Calls setinv function from makeCacheMatrix
                            ##setinv takes MI value in cacheSolve, use it as 
                            ##argument and gives MI in makeCacheMatrix the value
                            ##of MI in cacheSolve
    
    MI                      ##cacheSolve returns MI value (in cacheSolve)
}
