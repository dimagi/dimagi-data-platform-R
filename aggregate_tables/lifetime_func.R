library(dplyr)

# lifetime table functions

# calendar month
next.month <- function(d) as.Date(as.yearmon(d) + 1/12) +
  as.numeric(d - as.Date(as.yearmon(d)))


# percentage of visits in different time periods in a day
visitTimePercentage <- function(data){
    f <- function(x) {
        r1 <- data.frame(table(x$visit_time)/nrow(x))
        names(r1) <- c('time', 'proportion')
        r1$proportion <- round(100*r1$proportion, 2)
        return(r1)
    }
    return(group_by(data, user_id) %.% do(f))
}
reshapedVisitTime <- function(x){
  r2 <- dcast(x, x[,1]~x[,2])
  return(r2)
}

# Date difference function
dateDiff <- function(time1, time2){
  date_difference <- c(NA, diff(time1)) # this examines if sorted visits happen in the same or different days
  date_difference[which(is.na(time2))] <- NA # date_difference should be NA for the first visit by each unique worker
  return(date_difference)
}


# batch entry function
batchEntry <- function(time1, time2, n){ # n is the arbitrary break
  batch_entry <- ifelse(time1 == 0 & time2 < n, 1, 0)  # time_since_previous is calculated in seconds
  temp <- which(batch_entry == 1) # this gives the index of batch_entry visits
  batch_entry[temp - 1] <- 1 # this flags previous visit of a batch_entry visit as batch
  return(batch_entry)
}


# days elapsed between two visits to a case
daysElapsedCase <- function(id, date){
  dec <- c(NA, as.numeric(diff(date, units = "days")))
  dec[which(diff(as.numeric(factor(id, ordered = TRUE))) != 0) + 1] <- NA
  return(dec)
}


# if a visit is registering new case
newCase <- function(days){ # n: days elapased since the case was last visited
  new <- ifelse(is.na(days) == TRUE, 1, 0)
  return(new)
}


# total cases visited by mobile user in an hour
avgCasesPerHour <- function(id, date, hour) {
  y <- as.data.frame(table(id, date, hour))
  names(y) <- c("user_id", "visit_date", "visit_hour", "cases_visited_per_hour")
  y1 <- y[which(y$cases_visited_per_hour != 0),]  # exclude non-active visit_hour index
  y1_aggr <- aggregate(cases_visited_per_hour~user_id, data = y1, mean) # this returns the average number of cases that had interactions with user in an active hour
  names(y1_aggr) <- c("user_id", "avg_cases_visited_per_active_hour")
  #  y1_aggr$avg_cases_visited_per_active_hour <- round(y1_aggr$avg_cases_visited_per_active_hour, 2)
  return(y1_aggr)
}



# if a visit is touching a registered case
followUp <- function(days){ # n: days elapased since the case was last visited
  followup <- ifelse(is.na(days) == FALSE, 1, 0)
  return(followup)
}
