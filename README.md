airpollutant
============
##Part 2
##Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases.

complete <- function(directory, id = 1:332){
    files_list <- list.files(directory, full.names=TRUE)
    row_count <- length(id)  
    dat_c <- data.frame(id=1:row_count,nobs=1:row_count)
    dat_c[,1] <- id
    dat_comp <- data.frame() 
    j <- 1
    for (i in id){
        dat_comp <- read.csv(files_list[i])
        dat_c[j,2] <- sum(complete.cases(dat_comp[,2],dat_comp[,3]))
        j <- j + 1    
    }
    dat_c
}


##Part 3
##Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. The function should return a vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0.


corr <- function(directory, threshold = 0) {

    files_list <- list.files(directory, full.names=TRUE)
        
    dat_comp <- data.frame()
    j <- 1
    i <- 1
    for (i in 332){
        dat_comp <- read.csv(files_list[i])
        dat_test <- sum(complete.cases(dat_comp[,2],dat_comp[,3]))
            
        if (dat_test > threshold){
              j <- j + 1
        }
    }
    
    cr <- vector("numeric", length = j-1)
    
    k <- 1
    l <- 1
    for (k in 1:332){
      dat_comp <- read.csv(files_list[k])
      dat_test <- sum(complete.cases(dat_comp[,2],dat_comp[,3]))
      
      if (dat_test > threshold){
        cr[l] <- cor(dat_comp[,2],dat_comp[,3],use = "complete.obs")
        l <- l + 1
      }
    }  
  cr
}
