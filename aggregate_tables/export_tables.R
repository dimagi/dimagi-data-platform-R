
module <- c("visit", "lifetime", "monthly")

tableOut <- function(path, dat, x){
  filename <- paste(module[x], sep = "", ".csv")
  fullpath <- file.path(path,filename,fsep=.Platform$file.sep)
  write.csv(dat,fullpath)
}

