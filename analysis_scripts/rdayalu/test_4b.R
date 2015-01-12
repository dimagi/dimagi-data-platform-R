test_4b <- c()


#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_month_active == F)
  #Append "months to first attrition event" to the attrition list
  attrition_list <- lappend(attrition_list, rev(single_user$nvisits[1:attrition_positions[1]]))
  #Append "months to subsequent attrition events" to the attrition list
  if(length(attrition_positions)>1) {
    for(j in 2:length(attrition_positions)) {
      attrition_list <- lappend(attrition_list, rev(single_user$nvisits[(attrition_positions[j-1]+1):attrition_positions[j]]))
    }
  }
}
## Compute maximum length
max_length <- max(sapply(attrition_list, length))
## Add NA values to list elements
attrition_list <- lapply(attrition_list, function(v) { c(v, rep(NA, max_length-length(v)))})
## Create dataframe
attrition_data <- data.frame(do.call(rbind, attrition_list))
names(attrition_data) <- paste0("month_", 1:ncol(attrition_data))
#Keep rows with at least "N" months before attrition
#Here, N = 5
attrition_subset <- filter(attrition_data, !is.na(month_5))
attrition_subset <- select(attrition_subset, month_1, month_2, month_3, month_4, month_5)
#Calculate indicators per month relative to N = 5
attrition_subset$rel_1 <- (attrition_subset$month_1/attrition_subset$month_5)*100
attrition_subset$rel_2 <- (attrition_subset$month_2/attrition_subset$month_5)*100
attrition_subset$rel_3 <- (attrition_subset$month_3/attrition_subset$month_5)*100
attrition_subset$rel_4 <- (attrition_subset$month_4/attrition_subset$month_5)*100
attrition_subset$rel_5 <- (attrition_subset$month_5/attrition_subset$month_5)*100
is.na(attrition_subset) <- attrition_subset == "Inf"
attrition_subset <- filter(attrition_subset, !is.nan(rel_1) & !is.nan(rel_2) & !is.nan(rel_3) & !is.nan(rel_4) & !is.nan(rel_5))
#Test 4B: slope of line (lm) for realtive months 1-4 for each row
test_4b <- median(apply(attrition_subset[,6:9], 1, function(x) lm(x~c(1:4))$coefficients[[2]]))


#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_month_active == F)
  #Append "months to first attrition event" to the attrition list
  attrition_list <- lappend(attrition_list, rev(single_user$active_day_percent[1:attrition_positions[1]]))
  #Append "months to subsequent attrition events" to the attrition list
  if(length(attrition_positions)>1) {
    for(j in 2:length(attrition_positions)) {
      attrition_list <- lappend(attrition_list, rev(single_user$active_day_percent[(attrition_positions[j-1]+1):attrition_positions[j]]))
    }
  }
}
## Compute maximum length
max_length <- max(sapply(attrition_list, length))
## Add NA values to list elements
attrition_list <- lapply(attrition_list, function(v) { c(v, rep(NA, max_length-length(v)))})
## Create dataframe
attrition_data <- data.frame(do.call(rbind, attrition_list))
names(attrition_data) <- paste0("month_", 1:ncol(attrition_data))
#Keep rows with at least "N" months before attrition
#Here, N = 5
attrition_subset <- filter(attrition_data, !is.na(month_5))
attrition_subset <- select(attrition_subset, month_1, month_2, month_3, month_4, month_5)
#Calculate indicators per month relative to N = 5
attrition_subset$rel_1 <- (attrition_subset$month_1/attrition_subset$month_5)*100
attrition_subset$rel_2 <- (attrition_subset$month_2/attrition_subset$month_5)*100
attrition_subset$rel_3 <- (attrition_subset$month_3/attrition_subset$month_5)*100
attrition_subset$rel_4 <- (attrition_subset$month_4/attrition_subset$month_5)*100
attrition_subset$rel_5 <- (attrition_subset$month_5/attrition_subset$month_5)*100
is.na(attrition_subset) <- attrition_subset == "Inf"
attrition_subset <- filter(attrition_subset, !is.nan(rel_1) & !is.nan(rel_2) & !is.nan(rel_3) & !is.nan(rel_4) & !is.nan(rel_5))
#Test 4B: slope of line (lm) for realtive months 1-4 for each row
test_4b <- append(test_4b, 
                  median(apply(attrition_subset[,6:9], 1, function(x) lm(x~c(1:4))$coefficients[[2]])))



#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_month_active == F)
  #Append "months to first attrition event" to the attrition list
  attrition_list <- lappend(attrition_list, rev(single_user$nforms[1:attrition_positions[1]]))
  #Append "months to subsequent attrition events" to the attrition list
  if(length(attrition_positions)>1) {
    for(j in 2:length(attrition_positions)) {
      attrition_list <- lappend(attrition_list, rev(single_user$nforms[(attrition_positions[j-1]+1):attrition_positions[j]]))
    }
  }
}
## Compute maximum length
max_length <- max(sapply(attrition_list, length))
## Add NA values to list elements
attrition_list <- lapply(attrition_list, function(v) { c(v, rep(NA, max_length-length(v)))})
## Create dataframe
attrition_data <- data.frame(do.call(rbind, attrition_list))
names(attrition_data) <- paste0("month_", 1:ncol(attrition_data))
#Keep rows with at least "N" months before attrition
#Here, N = 5
attrition_subset <- filter(attrition_data, !is.na(month_5))
attrition_subset <- select(attrition_subset, month_1, month_2, month_3, month_4, month_5)
#Calculate indicators per month relative to N = 5
attrition_subset$rel_1 <- (attrition_subset$month_1/attrition_subset$month_5)*100
attrition_subset$rel_2 <- (attrition_subset$month_2/attrition_subset$month_5)*100
attrition_subset$rel_3 <- (attrition_subset$month_3/attrition_subset$month_5)*100
attrition_subset$rel_4 <- (attrition_subset$month_4/attrition_subset$month_5)*100
attrition_subset$rel_5 <- (attrition_subset$month_5/attrition_subset$month_5)*100
is.na(attrition_subset) <- attrition_subset == "Inf"
attrition_subset <- filter(attrition_subset, !is.nan(rel_1) & !is.nan(rel_2) & !is.nan(rel_3) & !is.nan(rel_4) & !is.nan(rel_5))
#Test 4B: slope of line (lm) for realtive months 1-4 for each row
test_4b <- append(test_4b, 
                  median(apply(attrition_subset[,6:9], 1, function(x) lm(x~c(1:4))$coefficients[[2]])))



#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_month_active == F)
  #Append "months to first attrition event" to the attrition list
  attrition_list <- lappend(attrition_list, rev(single_user$median_visit_duration[1:attrition_positions[1]]))
  #Append "months to subsequent attrition events" to the attrition list
  if(length(attrition_positions)>1) {
    for(j in 2:length(attrition_positions)) {
      attrition_list <- lappend(attrition_list, rev(single_user$median_visit_duration[(attrition_positions[j-1]+1):attrition_positions[j]]))
    }
  }
}
## Compute maximum length
max_length <- max(sapply(attrition_list, length))
## Add NA values to list elements
attrition_list <- lapply(attrition_list, function(v) { c(v, rep(NA, max_length-length(v)))})
## Create dataframe
attrition_data <- data.frame(do.call(rbind, attrition_list))
names(attrition_data) <- paste0("month_", 1:ncol(attrition_data))
#Keep rows with at least "N" months before attrition
#Here, N = 5
attrition_subset <- filter(attrition_data, !is.na(month_5))
attrition_subset <- select(attrition_subset, month_1, month_2, month_3, month_4, month_5)
#Calculate indicators per month relative to N = 5
attrition_subset$rel_1 <- (attrition_subset$month_1/attrition_subset$month_5)*100
attrition_subset$rel_2 <- (attrition_subset$month_2/attrition_subset$month_5)*100
attrition_subset$rel_3 <- (attrition_subset$month_3/attrition_subset$month_5)*100
attrition_subset$rel_4 <- (attrition_subset$month_4/attrition_subset$month_5)*100
attrition_subset$rel_5 <- (attrition_subset$month_5/attrition_subset$month_5)*100
is.na(attrition_subset) <- attrition_subset == "Inf"
attrition_subset <- filter(attrition_subset, !is.nan(rel_1) & !is.nan(rel_2) & !is.nan(rel_3) & !is.nan(rel_4) & !is.nan(rel_5))
#Test 4B: slope of line (lm) for realtive months 1-4 for each row
test_4b <- append(test_4b, 
                  median(apply(attrition_subset[,6:9], 1, function(x) lm(x~c(1:4))$coefficients[[2]])))


#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_month_active == F)
  #Append "months to first attrition event" to the attrition list
  attrition_list <- lappend(attrition_list, rev(single_user$median_visits_per_day[1:attrition_positions[1]]))
  #Append "months to subsequent attrition events" to the attrition list
  if(length(attrition_positions)>1) {
    for(j in 2:length(attrition_positions)) {
      attrition_list <- lappend(attrition_list, rev(single_user$median_visits_per_day[(attrition_positions[j-1]+1):attrition_positions[j]]))
    }
  }
}
## Compute maximum length
max_length <- max(sapply(attrition_list, length))
## Add NA values to list elements
attrition_list <- lapply(attrition_list, function(v) { c(v, rep(NA, max_length-length(v)))})
## Create dataframe
attrition_data <- data.frame(do.call(rbind, attrition_list))
names(attrition_data) <- paste0("month_", 1:ncol(attrition_data))
#Keep rows with at least "N" months before attrition
#Here, N = 5
attrition_subset <- filter(attrition_data, !is.na(month_5))
attrition_subset <- select(attrition_subset, month_1, month_2, month_3, month_4, month_5)
#Calculate indicators per month relative to N = 5
attrition_subset$rel_1 <- (attrition_subset$month_1/attrition_subset$month_5)*100
attrition_subset$rel_2 <- (attrition_subset$month_2/attrition_subset$month_5)*100
attrition_subset$rel_3 <- (attrition_subset$month_3/attrition_subset$month_5)*100
attrition_subset$rel_4 <- (attrition_subset$month_4/attrition_subset$month_5)*100
attrition_subset$rel_5 <- (attrition_subset$month_5/attrition_subset$month_5)*100
is.na(attrition_subset) <- attrition_subset == "Inf"
attrition_subset <- filter(attrition_subset, !is.nan(rel_1) & !is.nan(rel_2) & !is.nan(rel_3) & !is.nan(rel_4) & !is.nan(rel_5))
#Test 4B: slope of line (lm) for realtive months 1-4 for each row
test_4b <- append(test_4b, 
                  median(apply(attrition_subset[,6:9], 1, function(x) lm(x~c(1:4))$coefficients[[2]])))



#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_month_active == F)
  #Append "months to first attrition event" to the attrition list
  attrition_list <- lappend(attrition_list, rev(single_user$time_using_cc[1:attrition_positions[1]]))
  #Append "months to subsequent attrition events" to the attrition list
  if(length(attrition_positions)>1) {
    for(j in 2:length(attrition_positions)) {
      attrition_list <- lappend(attrition_list, rev(single_user$time_using_cc[(attrition_positions[j-1]+1):attrition_positions[j]]))
    }
  }
}
## Compute maximum length
max_length <- max(sapply(attrition_list, length))
## Add NA values to list elements
attrition_list <- lapply(attrition_list, function(v) { c(v, rep(NA, max_length-length(v)))})
## Create dataframe
attrition_data <- data.frame(do.call(rbind, attrition_list))
names(attrition_data) <- paste0("month_", 1:ncol(attrition_data))
#Keep rows with at least "N" months before attrition
#Here, N = 5
attrition_subset <- filter(attrition_data, !is.na(month_5))
attrition_subset <- select(attrition_subset, month_1, month_2, month_3, month_4, month_5)
#Calculate indicators per month relative to N = 5
attrition_subset$rel_1 <- (attrition_subset$month_1/attrition_subset$month_5)*100
attrition_subset$rel_2 <- (attrition_subset$month_2/attrition_subset$month_5)*100
attrition_subset$rel_3 <- (attrition_subset$month_3/attrition_subset$month_5)*100
attrition_subset$rel_4 <- (attrition_subset$month_4/attrition_subset$month_5)*100
attrition_subset$rel_5 <- (attrition_subset$month_5/attrition_subset$month_5)*100
is.na(attrition_subset) <- attrition_subset == "Inf"
attrition_subset <- filter(attrition_subset, !is.nan(rel_1) & !is.nan(rel_2) & !is.nan(rel_3) & !is.nan(rel_4) & !is.nan(rel_5))
#Test 4B: slope of line (lm) for realtive months 1-4 for each row
test_4b <- append(test_4b, 
                  median(apply(attrition_subset[,6:9], 1, function(x) lm(x~c(1:4))$coefficients[[2]])))


#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_month_active == F)
  #Append "months to first attrition event" to the attrition list
  attrition_list <- lappend(attrition_list, rev(single_user$ninteractions[1:attrition_positions[1]]))
  #Append "months to subsequent attrition events" to the attrition list
  if(length(attrition_positions)>1) {
    for(j in 2:length(attrition_positions)) {
      attrition_list <- lappend(attrition_list, rev(single_user$ninteractions[(attrition_positions[j-1]+1):attrition_positions[j]]))
    }
  }
}
## Compute maximum length
max_length <- max(sapply(attrition_list, length))
## Add NA values to list elements
attrition_list <- lapply(attrition_list, function(v) { c(v, rep(NA, max_length-length(v)))})
## Create dataframe
attrition_data <- data.frame(do.call(rbind, attrition_list))
names(attrition_data) <- paste0("month_", 1:ncol(attrition_data))
#Keep rows with at least "N" months before attrition
#Here, N = 5
attrition_subset <- filter(attrition_data, !is.na(month_5))
attrition_subset <- select(attrition_subset, month_1, month_2, month_3, month_4, month_5)
#Calculate indicators per month relative to N = 5
attrition_subset$rel_1 <- (attrition_subset$month_1/attrition_subset$month_5)*100
attrition_subset$rel_2 <- (attrition_subset$month_2/attrition_subset$month_5)*100
attrition_subset$rel_3 <- (attrition_subset$month_3/attrition_subset$month_5)*100
attrition_subset$rel_4 <- (attrition_subset$month_4/attrition_subset$month_5)*100
attrition_subset$rel_5 <- (attrition_subset$month_5/attrition_subset$month_5)*100
is.na(attrition_subset) <- attrition_subset == "Inf"
attrition_subset <- filter(attrition_subset, !is.nan(rel_1) & !is.nan(rel_2) & !is.nan(rel_3) & !is.nan(rel_4) & !is.nan(rel_5))
#Test 4B: slope of line (lm) for realtive months 1-4 for each row
test_4b <- append(test_4b, 
                  median(apply(attrition_subset[,6:9], 1, function(x) lm(x~c(1:4))$coefficients[[2]])))


#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_month_active == F)
  #Append "months to first attrition event" to the attrition list
  attrition_list <- lappend(attrition_list, rev(single_user$ncases_registered[1:attrition_positions[1]]))
  #Append "months to subsequent attrition events" to the attrition list
  if(length(attrition_positions)>1) {
    for(j in 2:length(attrition_positions)) {
      attrition_list <- lappend(attrition_list, rev(single_user$ncases_registered[(attrition_positions[j-1]+1):attrition_positions[j]]))
    }
  }
}
## Compute maximum length
max_length <- max(sapply(attrition_list, length))
## Add NA values to list elements
attrition_list <- lapply(attrition_list, function(v) { c(v, rep(NA, max_length-length(v)))})
## Create dataframe
attrition_data <- data.frame(do.call(rbind, attrition_list))
names(attrition_data) <- paste0("month_", 1:ncol(attrition_data))
#Keep rows with at least "N" months before attrition
#Here, N = 5
attrition_subset <- filter(attrition_data, !is.na(month_5))
attrition_subset <- select(attrition_subset, month_1, month_2, month_3, month_4, month_5)
#Calculate indicators per month relative to N = 5
attrition_subset$rel_1 <- (attrition_subset$month_1/attrition_subset$month_5)*100
attrition_subset$rel_2 <- (attrition_subset$month_2/attrition_subset$month_5)*100
attrition_subset$rel_3 <- (attrition_subset$month_3/attrition_subset$month_5)*100
attrition_subset$rel_4 <- (attrition_subset$month_4/attrition_subset$month_5)*100
attrition_subset$rel_5 <- (attrition_subset$month_5/attrition_subset$month_5)*100
is.na(attrition_subset) <- attrition_subset == "Inf"
attrition_subset <- filter(attrition_subset, !is.nan(rel_1) & !is.nan(rel_2) & !is.nan(rel_3) & !is.nan(rel_4) & !is.nan(rel_5))
#Test 4B: slope of line (lm) for realtive months 1-4 for each row
test_4b <- append(test_4b, 
                  median(apply(attrition_subset[,6:9], 1, function(x) lm(x~c(1:4))$coefficients[[2]])))


#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_month_active == F)
  #Append "months to first attrition event" to the attrition list
  attrition_list <- lappend(attrition_list, rev(single_user$register_followup[1:attrition_positions[1]]))
  #Append "months to subsequent attrition events" to the attrition list
  if(length(attrition_positions)>1) {
    for(j in 2:length(attrition_positions)) {
      attrition_list <- lappend(attrition_list, rev(single_user$register_followup[(attrition_positions[j-1]+1):attrition_positions[j]]))
    }
  }
}
## Compute maximum length
max_length <- max(sapply(attrition_list, length))
## Add NA values to list elements
attrition_list <- lapply(attrition_list, function(v) { c(v, rep(NA, max_length-length(v)))})
## Create dataframe
attrition_data <- data.frame(do.call(rbind, attrition_list))
names(attrition_data) <- paste0("month_", 1:ncol(attrition_data))
#Keep rows with at least "N" months before attrition
#Here, N = 5
attrition_subset <- filter(attrition_data, !is.na(month_5))
attrition_subset <- select(attrition_subset, month_1, month_2, month_3, month_4, month_5)
#Calculate indicators per month relative to N = 5
attrition_subset$rel_1 <- (attrition_subset$month_1/attrition_subset$month_5)*100
attrition_subset$rel_2 <- (attrition_subset$month_2/attrition_subset$month_5)*100
attrition_subset$rel_3 <- (attrition_subset$month_3/attrition_subset$month_5)*100
attrition_subset$rel_4 <- (attrition_subset$month_4/attrition_subset$month_5)*100
attrition_subset$rel_5 <- (attrition_subset$month_5/attrition_subset$month_5)*100
is.na(attrition_subset) <- attrition_subset == "Inf"
attrition_subset <- filter(attrition_subset, !is.nan(rel_1) & !is.nan(rel_2) & !is.nan(rel_3) & !is.nan(rel_4) & !is.nan(rel_5))
#Test 4B: slope of line (lm) for realtive months 1-4 for each row
test_4b <- append(test_4b, 
                  median(apply(attrition_subset[,6:9], 1, function(x) lm(x~c(1:4))$coefficients[[2]])))



#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_month_active == F)
  #Append "months to first attrition event" to the attrition list
  attrition_list <- lappend(attrition_list, rev(single_user$case_register_followup_rate[1:attrition_positions[1]]))
  #Append "months to subsequent attrition events" to the attrition list
  if(length(attrition_positions)>1) {
    for(j in 2:length(attrition_positions)) {
      attrition_list <- lappend(attrition_list, rev(single_user$case_register_followup_rate[(attrition_positions[j-1]+1):attrition_positions[j]]))
    }
  }
}
## Compute maximum length
max_length <- max(sapply(attrition_list, length))
## Add NA values to list elements
attrition_list <- lapply(attrition_list, function(v) { c(v, rep(NA, max_length-length(v)))})
## Create dataframe
attrition_data <- data.frame(do.call(rbind, attrition_list))
names(attrition_data) <- paste0("month_", 1:ncol(attrition_data))
#Keep rows with at least "N" months before attrition
#Here, N = 5
attrition_subset <- filter(attrition_data, !is.na(month_5))
attrition_subset <- select(attrition_subset, month_1, month_2, month_3, month_4, month_5)
#Calculate indicators per month relative to N = 5
attrition_subset$rel_1 <- (attrition_subset$month_1/attrition_subset$month_5)*100
attrition_subset$rel_2 <- (attrition_subset$month_2/attrition_subset$month_5)*100
attrition_subset$rel_3 <- (attrition_subset$month_3/attrition_subset$month_5)*100
attrition_subset$rel_4 <- (attrition_subset$month_4/attrition_subset$month_5)*100
attrition_subset$rel_5 <- (attrition_subset$month_5/attrition_subset$month_5)*100
is.na(attrition_subset) <- attrition_subset == "Inf"
attrition_subset <- filter(attrition_subset, !is.nan(rel_1) & !is.nan(rel_2) & !is.nan(rel_3) & !is.nan(rel_4) & !is.nan(rel_5))
#Test 4B: slope of line (lm) for realtive months 1-4 for each row
test_4b <- append(test_4b, 
                  median(apply(attrition_subset[,6:9], 1, function(x) lm(x~c(1:4))$coefficients[[2]])))



#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_month_active == F)
  #Append "months to first attrition event" to the attrition list
  attrition_list <- lappend(attrition_list, rev(single_user$ncases_touched[1:attrition_positions[1]]))
  #Append "months to subsequent attrition events" to the attrition list
  if(length(attrition_positions)>1) {
    for(j in 2:length(attrition_positions)) {
      attrition_list <- lappend(attrition_list, rev(single_user$ncases_touched[(attrition_positions[j-1]+1):attrition_positions[j]]))
    }
  }
}
## Compute maximum length
max_length <- max(sapply(attrition_list, length))
## Add NA values to list elements
attrition_list <- lapply(attrition_list, function(v) { c(v, rep(NA, max_length-length(v)))})
## Create dataframe
attrition_data <- data.frame(do.call(rbind, attrition_list))
names(attrition_data) <- paste0("month_", 1:ncol(attrition_data))
#Keep rows with at least "N" months before attrition
#Here, N = 5
attrition_subset <- filter(attrition_data, !is.na(month_5))
attrition_subset <- select(attrition_subset, month_1, month_2, month_3, month_4, month_5)
#Calculate indicators per month relative to N = 5
attrition_subset$rel_1 <- (attrition_subset$month_1/attrition_subset$month_5)*100
attrition_subset$rel_2 <- (attrition_subset$month_2/attrition_subset$month_5)*100
attrition_subset$rel_3 <- (attrition_subset$month_3/attrition_subset$month_5)*100
attrition_subset$rel_4 <- (attrition_subset$month_4/attrition_subset$month_5)*100
attrition_subset$rel_5 <- (attrition_subset$month_5/attrition_subset$month_5)*100
is.na(attrition_subset) <- attrition_subset == "Inf"
attrition_subset <- filter(attrition_subset, !is.nan(rel_1) & !is.nan(rel_2) & !is.nan(rel_3) & !is.nan(rel_4) & !is.nan(rel_5))
#Test 4B: slope of line (lm) for realtive months 1-4 for each row
test_4b <- append(test_4b, 
                  median(apply(attrition_subset[,6:9], 1, function(x) lm(x~c(1:4))$coefficients[[2]])))



#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_month_active == F)
  #Append "months to first attrition event" to the attrition list
  attrition_list <- lappend(attrition_list, rev(single_user$nunique_followups[1:attrition_positions[1]]))
  #Append "months to subsequent attrition events" to the attrition list
  if(length(attrition_positions)>1) {
    for(j in 2:length(attrition_positions)) {
      attrition_list <- lappend(attrition_list, rev(single_user$nunique_followups[(attrition_positions[j-1]+1):attrition_positions[j]]))
    }
  }
}
## Compute maximum length
max_length <- max(sapply(attrition_list, length))
## Add NA values to list elements
attrition_list <- lapply(attrition_list, function(v) { c(v, rep(NA, max_length-length(v)))})
## Create dataframe
attrition_data <- data.frame(do.call(rbind, attrition_list))
names(attrition_data) <- paste0("month_", 1:ncol(attrition_data))
#Keep rows with at least "N" months before attrition
#Here, N = 5
attrition_subset <- filter(attrition_data, !is.na(month_5))
attrition_subset <- select(attrition_subset, month_1, month_2, month_3, month_4, month_5)
#Calculate indicators per month relative to N = 5
attrition_subset$rel_1 <- (attrition_subset$month_1/attrition_subset$month_5)*100
attrition_subset$rel_2 <- (attrition_subset$month_2/attrition_subset$month_5)*100
attrition_subset$rel_3 <- (attrition_subset$month_3/attrition_subset$month_5)*100
attrition_subset$rel_4 <- (attrition_subset$month_4/attrition_subset$month_5)*100
attrition_subset$rel_5 <- (attrition_subset$month_5/attrition_subset$month_5)*100
is.na(attrition_subset) <- attrition_subset == "Inf"
attrition_subset <- filter(attrition_subset, !is.nan(rel_1) & !is.nan(rel_2) & !is.nan(rel_3) & !is.nan(rel_4) & !is.nan(rel_5))
#Test 4B: slope of line (lm) for realtive months 1-4 for each row
test_4b <- append(test_4b, 
                  median(apply(attrition_subset[,6:9], 1, function(x) lm(x~c(1:4))$coefficients[[2]])))



#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_month_active == F)
  #Append "months to first attrition event" to the attrition list
  attrition_list <- lappend(attrition_list, rev(single_user$audio_plays[1:attrition_positions[1]]))
  #Append "months to subsequent attrition events" to the attrition list
  if(length(attrition_positions)>1) {
    for(j in 2:length(attrition_positions)) {
      attrition_list <- lappend(attrition_list, rev(single_user$audio_plays[(attrition_positions[j-1]+1):attrition_positions[j]]))
    }
  }
}
## Compute maximum length
max_length <- max(sapply(attrition_list, length))
## Add NA values to list elements
attrition_list <- lapply(attrition_list, function(v) { c(v, rep(NA, max_length-length(v)))})
## Create dataframe
attrition_data <- data.frame(do.call(rbind, attrition_list))
names(attrition_data) <- paste0("month_", 1:ncol(attrition_data))
#Keep rows with at least "N" months before attrition
#Here, N = 5
attrition_subset <- filter(attrition_data, !is.na(month_5))
attrition_subset <- select(attrition_subset, month_1, month_2, month_3, month_4, month_5)
#Calculate indicators per month relative to N = 5
attrition_subset$rel_1 <- (attrition_subset$month_1/attrition_subset$month_5)*100
attrition_subset$rel_2 <- (attrition_subset$month_2/attrition_subset$month_5)*100
attrition_subset$rel_3 <- (attrition_subset$month_3/attrition_subset$month_5)*100
attrition_subset$rel_4 <- (attrition_subset$month_4/attrition_subset$month_5)*100
attrition_subset$rel_5 <- (attrition_subset$month_5/attrition_subset$month_5)*100
is.na(attrition_subset) <- attrition_subset == "Inf"
attrition_subset <- filter(attrition_subset, !is.nan(rel_1) & !is.nan(rel_2) & !is.nan(rel_3) & !is.nan(rel_4) & !is.nan(rel_5))
#Test 4B: slope of line (lm) for realtive months 1-4 for each row
test_4b <- append(test_4b, 
                  median(apply(attrition_subset[,6:9], 1, function(x) lm(x~c(1:4))$coefficients[[2]])))



#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_month_active == F)
  #Append "months to first attrition event" to the attrition list
  attrition_list <- lappend(attrition_list, rev(single_user$network_warnings[1:attrition_positions[1]]))
  #Append "months to subsequent attrition events" to the attrition list
  if(length(attrition_positions)>1) {
    for(j in 2:length(attrition_positions)) {
      attrition_list <- lappend(attrition_list, rev(single_user$network_warnings[(attrition_positions[j-1]+1):attrition_positions[j]]))
    }
  }
}
## Compute maximum length
max_length <- max(sapply(attrition_list, length))
## Add NA values to list elements
attrition_list <- lapply(attrition_list, function(v) { c(v, rep(NA, max_length-length(v)))})
## Create dataframe
attrition_data <- data.frame(do.call(rbind, attrition_list))
names(attrition_data) <- paste0("month_", 1:ncol(attrition_data))
#Keep rows with at least "N" months before attrition
#Here, N = 5
attrition_subset <- filter(attrition_data, !is.na(month_5))
attrition_subset <- select(attrition_subset, month_1, month_2, month_3, month_4, month_5)
#Calculate indicators per month relative to N = 5
attrition_subset$rel_1 <- (attrition_subset$month_1/attrition_subset$month_5)*100
attrition_subset$rel_2 <- (attrition_subset$month_2/attrition_subset$month_5)*100
attrition_subset$rel_3 <- (attrition_subset$month_3/attrition_subset$month_5)*100
attrition_subset$rel_4 <- (attrition_subset$month_4/attrition_subset$month_5)*100
attrition_subset$rel_5 <- (attrition_subset$month_5/attrition_subset$month_5)*100
is.na(attrition_subset) <- attrition_subset == "Inf"
attrition_subset <- filter(attrition_subset, !is.nan(rel_1) & !is.nan(rel_2) & !is.nan(rel_3) & !is.nan(rel_4) & !is.nan(rel_5))
#Test 4B: slope of line (lm) for realtive months 1-4 for each row
test_4b <- append(test_4b, 
                  median(apply(attrition_subset[,6:9], 1, function(x) lm(x~c(1:4))$coefficients[[2]])))



#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_month_active == F)
  #Append "months to first attrition event" to the attrition list
  attrition_list <- lappend(attrition_list, rev(single_user$num_user_pk[1:attrition_positions[1]]))
  #Append "months to subsequent attrition events" to the attrition list
  if(length(attrition_positions)>1) {
    for(j in 2:length(attrition_positions)) {
      attrition_list <- lappend(attrition_list, rev(single_user$num_user_pk[(attrition_positions[j-1]+1):attrition_positions[j]]))
    }
  }
}
## Compute maximum length
max_length <- max(sapply(attrition_list, length))
## Add NA values to list elements
attrition_list <- lapply(attrition_list, function(v) { c(v, rep(NA, max_length-length(v)))})
## Create dataframe
attrition_data <- data.frame(do.call(rbind, attrition_list))
names(attrition_data) <- paste0("month_", 1:ncol(attrition_data))
#Keep rows with at least "N" months before attrition
#Here, N = 5
attrition_subset <- filter(attrition_data, !is.na(month_5))
attrition_subset <- select(attrition_subset, month_1, month_2, month_3, month_4, month_5)
#Calculate indicators per month relative to N = 5
attrition_subset$rel_1 <- (attrition_subset$month_1/attrition_subset$month_5)*100
attrition_subset$rel_2 <- (attrition_subset$month_2/attrition_subset$month_5)*100
attrition_subset$rel_3 <- (attrition_subset$month_3/attrition_subset$month_5)*100
attrition_subset$rel_4 <- (attrition_subset$month_4/attrition_subset$month_5)*100
attrition_subset$rel_5 <- (attrition_subset$month_5/attrition_subset$month_5)*100
is.na(attrition_subset) <- attrition_subset == "Inf"
attrition_subset <- filter(attrition_subset, !is.nan(rel_1) & !is.nan(rel_2) & !is.nan(rel_3) & !is.nan(rel_4) & !is.nan(rel_5))
#Test 4B: slope of line (lm) for realtive months 1-4 for each row
test_4b <- append(test_4b, 
                  median(apply(attrition_subset[,6:9], 1, function(x) lm(x~c(1:4))$coefficients[[2]])))



#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_month_active == F)
  #Append "months to first attrition event" to the attrition list
  attrition_list <- lappend(attrition_list, rev(single_user$domain_numeric[1:attrition_positions[1]]))
  #Append "months to subsequent attrition events" to the attrition list
  if(length(attrition_positions)>1) {
    for(j in 2:length(attrition_positions)) {
      attrition_list <- lappend(attrition_list, rev(single_user$domain_numeric[(attrition_positions[j-1]+1):attrition_positions[j]]))
    }
  }
}
## Compute maximum length
max_length <- max(sapply(attrition_list, length))
## Add NA values to list elements
attrition_list <- lapply(attrition_list, function(v) { c(v, rep(NA, max_length-length(v)))})
## Create dataframe
attrition_data <- data.frame(do.call(rbind, attrition_list))
names(attrition_data) <- paste0("month_", 1:ncol(attrition_data))
#Keep rows with at least "N" months before attrition
#Here, N = 5
attrition_subset <- filter(attrition_data, !is.na(month_5))
attrition_subset <- select(attrition_subset, month_1, month_2, month_3, month_4, month_5)
#Calculate indicators per month relative to N = 5
attrition_subset$rel_1 <- (attrition_subset$month_1/attrition_subset$month_5)*100
attrition_subset$rel_2 <- (attrition_subset$month_2/attrition_subset$month_5)*100
attrition_subset$rel_3 <- (attrition_subset$month_3/attrition_subset$month_5)*100
attrition_subset$rel_4 <- (attrition_subset$month_4/attrition_subset$month_5)*100
attrition_subset$rel_5 <- (attrition_subset$month_5/attrition_subset$month_5)*100
is.na(attrition_subset) <- attrition_subset == "Inf"
attrition_subset <- filter(attrition_subset, !is.nan(rel_1) & !is.nan(rel_2) & !is.nan(rel_3) & !is.nan(rel_4) & !is.nan(rel_5))
#Test 4B: slope of line (lm) for realtive months 1-4 for each row
test_4b <- append(test_4b, 
                  median(apply(attrition_subset[,6:9], 1, function(x) lm(x~c(1:4))$coefficients[[2]])))



#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_month_active == F)
  #Append "months to first attrition event" to the attrition list
  attrition_list <- lappend(attrition_list, rev(single_user$sample_undefined[1:attrition_positions[1]]))
  #Append "months to subsequent attrition events" to the attrition list
  if(length(attrition_positions)>1) {
    for(j in 2:length(attrition_positions)) {
      attrition_list <- lappend(attrition_list, rev(single_user$sample_undefined[(attrition_positions[j-1]+1):attrition_positions[j]]))
    }
  }
}
## Compute maximum length
max_length <- max(sapply(attrition_list, length))
## Add NA values to list elements
attrition_list <- lapply(attrition_list, function(v) { c(v, rep(NA, max_length-length(v)))})
## Create dataframe
attrition_data <- data.frame(do.call(rbind, attrition_list))
names(attrition_data) <- paste0("month_", 1:ncol(attrition_data))
#Keep rows with at least "N" months before attrition
#Here, N = 5
attrition_subset <- filter(attrition_data, !is.na(month_5))
attrition_subset <- select(attrition_subset, month_1, month_2, month_3, month_4, month_5)
#Calculate indicators per month relative to N = 5
attrition_subset$rel_1 <- (attrition_subset$month_1/attrition_subset$month_5)*100
attrition_subset$rel_2 <- (attrition_subset$month_2/attrition_subset$month_5)*100
attrition_subset$rel_3 <- (attrition_subset$month_3/attrition_subset$month_5)*100
attrition_subset$rel_4 <- (attrition_subset$month_4/attrition_subset$month_5)*100
attrition_subset$rel_5 <- (attrition_subset$month_5/attrition_subset$month_5)*100
is.na(attrition_subset) <- attrition_subset == "Inf"
attrition_subset <- filter(attrition_subset, !is.nan(rel_1) & !is.nan(rel_2) & !is.nan(rel_3) & !is.nan(rel_4) & !is.nan(rel_5))
#Test 4B: slope of line (lm) for realtive months 1-4 for each row
test_4b <- append(test_4b, 
                  median(apply(attrition_subset[,6:9], 1, function(x) lm(x~c(1:4))$coefficients[[2]])))



#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_month_active == F)
  #Append "months to first attrition event" to the attrition list
  attrition_list <- lappend(attrition_list, rev(single_user$sample_normal[1:attrition_positions[1]]))
  #Append "months to subsequent attrition events" to the attrition list
  if(length(attrition_positions)>1) {
    for(j in 2:length(attrition_positions)) {
      attrition_list <- lappend(attrition_list, rev(single_user$sample_normal[(attrition_positions[j-1]+1):attrition_positions[j]]))
    }
  }
}
## Compute maximum length
max_length <- max(sapply(attrition_list, length))
## Add NA values to list elements
attrition_list <- lapply(attrition_list, function(v) { c(v, rep(NA, max_length-length(v)))})
## Create dataframe
attrition_data <- data.frame(do.call(rbind, attrition_list))
names(attrition_data) <- paste0("month_", 1:ncol(attrition_data))
#Keep rows with at least "N" months before attrition
#Here, N = 5
attrition_subset <- filter(attrition_data, !is.na(month_5))
attrition_subset <- select(attrition_subset, month_1, month_2, month_3, month_4, month_5)
#Calculate indicators per month relative to N = 5
attrition_subset$rel_1 <- (attrition_subset$month_1/attrition_subset$month_5)*100
attrition_subset$rel_2 <- (attrition_subset$month_2/attrition_subset$month_5)*100
attrition_subset$rel_3 <- (attrition_subset$month_3/attrition_subset$month_5)*100
attrition_subset$rel_4 <- (attrition_subset$month_4/attrition_subset$month_5)*100
attrition_subset$rel_5 <- (attrition_subset$month_5/attrition_subset$month_5)*100
is.na(attrition_subset) <- attrition_subset == "Inf"
attrition_subset <- filter(attrition_subset, !is.nan(rel_1) & !is.nan(rel_2) & !is.nan(rel_3) & !is.nan(rel_4) & !is.nan(rel_5))
#Test 4B: slope of line (lm) for realtive months 1-4 for each row
test_4b <- append(test_4b, 
                  median(apply(attrition_subset[,6:9], 1, function(x) lm(x~c(1:4))$coefficients[[2]])))



#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_month_active == F)
  #Append "months to first attrition event" to the attrition list
  attrition_list <- lappend(attrition_list, rev(single_user$sample_percentile[1:attrition_positions[1]]))
  #Append "months to subsequent attrition events" to the attrition list
  if(length(attrition_positions)>1) {
    for(j in 2:length(attrition_positions)) {
      attrition_list <- lappend(attrition_list, rev(single_user$sample_percentile[(attrition_positions[j-1]+1):attrition_positions[j]]))
    }
  }
}
## Compute maximum length
max_length <- max(sapply(attrition_list, length))
## Add NA values to list elements
attrition_list <- lapply(attrition_list, function(v) { c(v, rep(NA, max_length-length(v)))})
## Create dataframe
attrition_data <- data.frame(do.call(rbind, attrition_list))
names(attrition_data) <- paste0("month_", 1:ncol(attrition_data))
#Keep rows with at least "N" months before attrition
#Here, N = 5
attrition_subset <- filter(attrition_data, !is.na(month_5))
attrition_subset <- select(attrition_subset, month_1, month_2, month_3, month_4, month_5)
#Calculate indicators per month relative to N = 5
attrition_subset$rel_1 <- (attrition_subset$month_1/attrition_subset$month_5)*100
attrition_subset$rel_2 <- (attrition_subset$month_2/attrition_subset$month_5)*100
attrition_subset$rel_3 <- (attrition_subset$month_3/attrition_subset$month_5)*100
attrition_subset$rel_4 <- (attrition_subset$month_4/attrition_subset$month_5)*100
attrition_subset$rel_5 <- (attrition_subset$month_5/attrition_subset$month_5)*100
is.na(attrition_subset) <- attrition_subset == "Inf"
attrition_subset <- filter(attrition_subset, !is.nan(rel_1) & !is.nan(rel_2) & !is.nan(rel_3) & !is.nan(rel_4) & !is.nan(rel_5))
#Test 4B: slope of line (lm) for realtive months 1-4 for each row
test_4b <- append(test_4b, 
                  median(apply(attrition_subset[,6:9], 1, function(x) lm(x~c(1:4))$coefficients[[2]])))



#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_month_active == F)
  #Append "months to first attrition event" to the attrition list
  attrition_list <- lappend(attrition_list, rev(single_user$sample_increase[1:attrition_positions[1]]))
  #Append "months to subsequent attrition events" to the attrition list
  if(length(attrition_positions)>1) {
    for(j in 2:length(attrition_positions)) {
      attrition_list <- lappend(attrition_list, rev(single_user$sample_increase[(attrition_positions[j-1]+1):attrition_positions[j]]))
    }
  }
}
## Compute maximum length
max_length <- max(sapply(attrition_list, length))
## Add NA values to list elements
attrition_list <- lapply(attrition_list, function(v) { c(v, rep(NA, max_length-length(v)))})
## Create dataframe
attrition_data <- data.frame(do.call(rbind, attrition_list))
names(attrition_data) <- paste0("month_", 1:ncol(attrition_data))
#Keep rows with at least "N" months before attrition
#Here, N = 5
attrition_subset <- filter(attrition_data, !is.na(month_5))
attrition_subset <- select(attrition_subset, month_1, month_2, month_3, month_4, month_5)
#Calculate indicators per month relative to N = 5
attrition_subset$rel_1 <- (attrition_subset$month_1/attrition_subset$month_5)*100
attrition_subset$rel_2 <- (attrition_subset$month_2/attrition_subset$month_5)*100
attrition_subset$rel_3 <- (attrition_subset$month_3/attrition_subset$month_5)*100
attrition_subset$rel_4 <- (attrition_subset$month_4/attrition_subset$month_5)*100
attrition_subset$rel_5 <- (attrition_subset$month_5/attrition_subset$month_5)*100
is.na(attrition_subset) <- attrition_subset == "Inf"
attrition_subset <- filter(attrition_subset, !is.nan(rel_1) & !is.nan(rel_2) & !is.nan(rel_3) & !is.nan(rel_4) & !is.nan(rel_5))
#Test 4B: slope of line (lm) for realtive months 1-4 for each row
test_4b <- append(test_4b, 
                  median(apply(attrition_subset[,6:9], 1, function(x) lm(x~c(1:4))$coefficients[[2]])))



#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_month_active == F)
  #Append "months to first attrition event" to the attrition list
  attrition_list <- lappend(attrition_list, rev(single_user$sample_decrease[1:attrition_positions[1]]))
  #Append "months to subsequent attrition events" to the attrition list
  if(length(attrition_positions)>1) {
    for(j in 2:length(attrition_positions)) {
      attrition_list <- lappend(attrition_list, rev(single_user$sample_decrease[(attrition_positions[j-1]+1):attrition_positions[j]]))
    }
  }
}
## Compute maximum length
max_length <- max(sapply(attrition_list, length))
## Add NA values to list elements
attrition_list <- lapply(attrition_list, function(v) { c(v, rep(NA, max_length-length(v)))})
## Create dataframe
attrition_data <- data.frame(do.call(rbind, attrition_list))
names(attrition_data) <- paste0("month_", 1:ncol(attrition_data))
#Keep rows with at least "N" months before attrition
#Here, N = 5
attrition_subset <- filter(attrition_data, !is.na(month_5))
attrition_subset <- select(attrition_subset, month_1, month_2, month_3, month_4, month_5)
#Calculate indicators per month relative to N = 5
attrition_subset$rel_1 <- (attrition_subset$month_1/attrition_subset$month_5)*100
attrition_subset$rel_2 <- (attrition_subset$month_2/attrition_subset$month_5)*100
attrition_subset$rel_3 <- (attrition_subset$month_3/attrition_subset$month_5)*100
attrition_subset$rel_4 <- (attrition_subset$month_4/attrition_subset$month_5)*100
attrition_subset$rel_5 <- (attrition_subset$month_5/attrition_subset$month_5)*100
is.na(attrition_subset) <- attrition_subset == "Inf"
attrition_subset <- filter(attrition_subset, !is.nan(rel_1) & !is.nan(rel_2) & !is.nan(rel_3) & !is.nan(rel_4) & !is.nan(rel_5))
#Test 4B: slope of line (lm) for realtive months 1-4 for each row
test_4b <- append(test_4b, 
                  median(apply(attrition_subset[,6:9], 1, function(x) lm(x~c(1:4))$coefficients[[2]])))
