add2 <- function(x,y){
  x + y
}

above10 < function(x){
  use <- x>10
  x[use] 
}

above <- function(x,n=10){
    use <- x > n
    x[use]
  
}

columnmean <- function(x, removeNA = TRUE){
    nc <- ncol(x)
    means <- numeric(nc)
    for (i in 1:nc){
        means[i] <- mean(x[,i], na.rm = removeNA)
    }

    means 
}


make.power <- function(n){
    pow <- function(x){
        x^n
    }
    pow 
}

y <- 10
f <- function(x){
    y <- 2
    y^2 + g(x)
}

g<- function(x){
    x*y
}

#Date class
x <- as.Date("1970-01-01")
unclass(x)
x <- as.Date("1970-01-02")
unclass(x)

#POSIXct 
#POSIXlt 
#weekdays 
#months 
#quarters "Q1","Q2","Q3","Q4"

x <- Sys.time()
p <- as.POSIXlt(x)
names(unclass(p))
p$sec


x <- Sys.time()
uncless(x)

datestring <- c("January 10, 2012 10:40", "December 9, 2011")
x <- strptime(datestring, "%B %d, %Y %H:%M")

class(x)


x <- as.Date("2012-03-01")
y <- as.Date("2012-02-28")

x - y 

x <- as.POSIXct("2012-10-25 01:00:00")
y <- as.POSIXct("2012-10-25 06:00:00", tz ="GMT")

y-x