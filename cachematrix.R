## For this assignment, build two function that play around enviroments. 
## 1st function, makeCacheMatrix, obtains the inverse of a matrix putting the outputs in cache so it can be called from somewhere else.
## 2nd function, cacheinverse, compares if the parameter matrix is the same of the one wich previously its inverse has been obtained
## and kept in cache, if so directly returns de inverse if not then calculates thhe inverse.  

## Calculate the inverse of a matrix and keeps the results in cache

makeCacheMatrix<- function(x=matrix()){
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinverse<-function(solve) m<<-solve
        getinverse<-function() m
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Checks if the matrix is the same, by the instruction isTRUE(identical(x,m)) and if not calculates its inverse.

cacheinverse<-function(x, ...){
        m=x$getinverse()
        if (isTRUE(identical(x,m))){
                message ("getting cached data")
                return(m)
        }
        
        data<-x$get()
        m<-solve(data, ...)
        x$setinverse(m)
        m
}
