
module <- c("visit", "lifetime", "monthly")

tableOut <- function(data, x){
  filename <- paste(module[x], sep = "", ".csv")
  write.csv(data, filename)
}

