# We've decided to separate the code in two source files to be more lisible

# clean_data - function which aims to delete columns which are not used in
# this project and delete lines with non-useful values
# @param data : dataset to clean
# @param col_to_keep : columns of the dataset which will be kept
# @param to_delete : values which will be considered as non-useful
clean_data <- function(data, col_to_keep, to_delete){
  print("Processing data clearing, please wait...")
  #Reducing dataframe without unused columns
  data<-data[1:nrow(data),col_to_keep]

  #Delete data which match with to_delete vector
  for (i in 1:length(to_delete)){
    for (j in col_to_keep){
      for (z in 1:nrow(data[j])){
        data[z,j] <- ifelse(data[z,j]==to_delete[i], NA, data[z,j])
      }
    }
  }
  
  #Throwing away each line with at least one NA
  for (j in col_to_keep){
    data <- data[is.na(data[j])==FALSE&is.null(data[j])==FALSE,]
  }
  return(data)
}


# make_an_ic : function which aims to build a confidence interval for theorical mean
# @param d : dataset
# @param alpha : probability that the mean comes out the ic
make_an_ic <- function(d, alpha){
  d <- strtoi(d)
  z_alpha <- qnorm(1 - alpha/2) 

  low <- mean(d) - z_alpha*sd(d)/sqrt(length(d))
  up <- mean(d) + z_alpha*sd(d)/sqrt(length(d))
  
  rtrn <- list(c(low, up), alpha)
  names(rtrn) <- c("ic", "alpha")
  return (rtrn)
}

# je doute de mon test de moyennes.. 
# quelqu'un pour m'aider à verifier la logique mathématique ?
are_means_different <- function(d1, d2, alpha){
  d1 <- strtoi(d1)
  d2 <- strtoi(d2)
  
  stest <- (mean(d1) - mean(d2))/(sqrt(var(d1)/length(d1) + var(d2)/length(d2)))
  z_alpha <- qnorm(1 - alpha/2)
  pvalue <- 2*(1 - pnorm(abs(stest)))
  result <- !(abs(stest) <= z_alpha)
  
  rtrn <- list(result, z_alpha, stest, pvalue, alpha)
  names(rtrn) <- c("result", "z_alpha", "stest", "pvalue", "alpha")
  return (rtrn)
}


