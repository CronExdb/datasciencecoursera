pollutantmean <- function(directory, pollutant, id = 1:332) {
  filelist <- list.files(directory, full.names = TRUE)
  values <- NA
  for (i in id) {
    data <- read.csv(filelist[i])
    values <- c(values, data[[pollutant]])
  }

  mean(values, na.rm = TRUE)
}


pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")





complete <- function(directory, id = 1:332) {
  filelist <- list.files(directory, full.names = TRUE)
  nobs <- c()
  for (i in id) {
    data <- read.csv(filelist[i])
    nobs <- c(nobs, sum(complete.cases(data)))
  }
  # d<-c(id, nobs)
  # return(d)
  data.frame(id, nobs)
}

complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)
cc <- complete("specdata", 54)
print(cc$nobs)
RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])








corr <- function(directory, threshold = 0) {
  filelist <- list.files(directory, full.names = TRUE)
  vec <- c()
  for(i in 1:332){
    data<-(read.csv(filelist[i]))
    compcase <-sum(complete.cases(data))
    if (compcase > threshold){
      
      data <- data[complete.cases(data[c('sulfate', 'nitrate')]),]
      
      vec <- c(vec, cor(data$sulfate, data$nitrate))
    
    } 
     
  }
  return(vec) 
}
  
debug(corr)  
cr <- corr("specdata", 150)
head(cr)
summary(cr)

cr <- corr("specdata", 400)
head(cr)
summary(cr)

cr <- corr("specdata", 5000)
summary(cr)  
length(cr)


cr <- corr("specdata")
summary(cr)
length(cr)


cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)


cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))







