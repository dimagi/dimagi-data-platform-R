# this function returns interaction data of each domain to compute aggregate table instead of being operated in a list


get_interaction_subset <- function(data, name){
  subset <- data[which(data$domain == name), ] # this returns the data frame of a specified domain
  return(subset)
}


