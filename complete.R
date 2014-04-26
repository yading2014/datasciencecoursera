complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating the location of the CSV files
    seq <- vector()
    number <- vector()
    
    for(i in id) {        
        #print(control)
        if (i<10){
            fileID <- paste("00", i, sep="")
        }
        if (i>9 && i<100){
            fileID <- paste("0", i, sep="") 
        }
        if (i>99){
            fileID <- i  
        }
        filePath <- paste(directory, fileID, sep="/")
        filePath <- paste(filePath,"csv",sep=".")   
        
        x <- read.csv(filePath)  

        data1 <- x$sulfate
        data2 <- x$nitrate
        
        data <- cbind(data1,data2)
        #print(data[,1])
        empty1 <- is.na(data[,1])
        empty2 <- is.na(data[,2])
        count <- 0
        for(j in 1:length(empty1)){
            if ((empty1[j] == FALSE) &&  (empty2[j] == FALSE)){
                count <- count + 1 
            } 
        }
        seq <- c(seq,i)
        number <- c(number, count)
        final_result <- data.frame(seq, number)           
    }
    
names(final_result) <- c("id","nobs")
print(final_result)

## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
}