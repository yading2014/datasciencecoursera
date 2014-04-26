corr <- function(directory, threshold = 0) {
    id = 1:332
    corrcoef <- vector()
    for(i in id) {
        input1 <- vector()
        input2 <- vector()
        count <- 0
        
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

        #print(data1)
        #print(data2)

        empty1 <- is.na(data1)
        empty2 <- is.na(data2)
        
        for(j in 1:length(empty1)){
            if ((empty1[j] == FALSE) &&  (empty2[j] == FALSE)){
                input1 <- c(input1, data1[j])
                input2 <- c(input2, data2[j])
                count <- count + 1
            }
        }
        if (count>threshold){
            coef <- cor(input1,input2)
            corrcoef <- c(corrcoef, coef)  
        }
    }
    corrcoef
    #output <- as.numeric(sprintf("%.5f", corrcoef))
    #output
    #as.numeric(sprintf("%.5f", corrcoef))   
}

