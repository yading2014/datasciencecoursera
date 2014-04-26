pollutantmean <- function(directory, pollutant, id = 1:332) {
    #'directory' is a character vector of length 1 indicating the location of the CSV files
    means <- numeric(length(id))
    control <- 1
    result <- vector()
    for(i in id) {        
        #print(control)
        if (i<10){
            fileID <- paste("00",i, sep="")
        }
        if (i>9 && i<100){
            fileID <- paste("0", i, sep="") 
        }
        if (i>99){
            fileID <- i  
        }
        filePath <- paste(directory, fileID, sep="/")
        filePath <- paste(filePath,"csv",sep=".")        
        #print(filePath)
        
        x <- read.csv(filePath)  
        #print(x)
        y <- x[[pollutant]]
        empty <- is.na(y)
        y <- y[!empty]
        #print(y)
        #print(nrow(y))
        #print(ncol(y))
  
        result <- c(result,y)
        
        #print(mean(y,na.rm=TRUE))
        #means[control] <- mean(y,na.rm=TRUE) 
        
        #print(mean(y, na.rm=FALSE))     

        #print(x) 
        #means[control] <- mean(x[pollutant], na.rm=TRUE)
        #print(means)
        #control <- control + 1
    }
    
#print(result)
as.numeric(sprintf("%.3f", mean(result)))
#print(mean(means))
#print(result) 
#print(mean(result))


    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
  
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
  
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
}
