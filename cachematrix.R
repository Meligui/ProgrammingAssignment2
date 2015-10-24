#The purpose of the following two functions is to create a matrix and allow the inverse of the matrix to be cached to avoid 
#recalculating the inverse every time it is needed. This allows the user to calculate the inverse of the matrix only once
#and thus avoid repeating complex computation. The matrix has to be square and has to be invertible for the inverse to be calculated 


#This function creates a matrix and stores it, you could also store the inverse of the matrix and retrieve it when it is needed
#The function can create a matrix of any size

makeCacheMatrix <- function(x = matrix()) {
 #initiate the inverse matrix
    inverse <- matrix(nrow=nrow(x),ncol=ncol(x))
    
    #set function allows creating and storing your matrix
    set <- function(refer){
        x <<- refer
        inverse <<- matrix(nrow=nrow(refer), ncol=ncol(refer)) #resets inverse every time matrix is changed
    }
    
    #get function allows retrieving the stored matrix
    get <- function(){
        x
    }
    
    #setInverse allows storing the inverse matrix
    setInverse <- function(I){
        inverse<<-I
    }
    
    #getInverse allows retrieving the inverse matrix
    getInverse <- function(){
        inverse
    }
    
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


#This function takes as an input an object created by the makeCacheMatrix function, it checks if the inverse is 
#already stored, if it is stored it retrieves it from the cache if it isn't stored it will calculate it and then store it 
#to be available for future use

cacheSolve <- function(x, ...) {
    I <- x$getInverse() #get the stored inverse
    InverseNA<-sapply(I,is.na) 
    if(InverseNA[1]==FALSE){ #return the stored inverse if it is not NA
        message("retrieving inverse") #return a message to indicate that it is retrieving and not calculating the inverse
        return(I) #return the inverse, exit the function
    }
    
    invertMatrix <- x$get() #retrieve stored matrix
    I<-solve(invertMatrix) #calculate inverse
    x$setInverse(I) #store inverse matrix
    I #return the inverse
}
