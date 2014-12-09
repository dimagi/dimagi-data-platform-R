#------------------------------------------------------------------------#
#Data for attrition blog based on Neal's attrition document
#------------------------------------------------------------------------#
training_typical <- arrange(training_typical, user_pk, calendar_month)
users <- unique(training_typical$user_pk)

#ncases_touched
leadup <- data.frame(matrix(ncol = 11, nrow = 1)) 
names(leadup) <- c("user_pk", "calendar_month", "previous_three_months_active", 
                   "next_month_active", "next_two_months_active", 
                   "next_three_months_active", "month_1", "month_2", "month_3", "month_4", 
                   "diff_days")
leadup$calendar_month <- as.Date(leadup$calendar_month)
leadup$user_pk <- as.numeric(leadup$user_pk)
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  single_user$calendar_month <- as.Date(single_user$calendar_month)
  for (j in 1:(nrow(single_user)-3)) {
    leadup_single <- data.frame(matrix(ncol = 11, nrow = 1)) 
    names(leadup_single) <- c("user_pk", "calendar_month", "previous_three_months_active", 
                              "next_month_active", "next_two_months_active", 
                              "next_three_months_active", "month_1", "month_2", "month_3", 
                              "month_4", "diff_days")
    leadup_single$user_pk <- as.numeric(single_user$user_pk[1])
    leadup_single$calendar_month <- as.Date(single_user$calendar_month[3+j])
    leadup_single$previous_three_months_active <- single_user$previous_three_months_active[3+j]
    leadup_single$next_month_active <- single_user$next_month_active[3+j]
    leadup_single$next_two_months_active <- single_user$next_two_months_active[3+j]
    leadup_single$next_three_months_active <- single_user$next_three_months_active[3+j]
    leadup_single$month_1 <- single_user$ncases_touched[3+j]
    leadup_single$month_2 <- single_user$ncases_touched[2+j]
    leadup_single$month_3 <- single_user$ncases_touched[1+j]
    leadup_single$month_4 <- single_user$ncases_touched[j]
    leadup_single$diff_days <- as.numeric(single_user$calendar_month[3+j] - single_user$calendar_month[j])
    leadup <- rbind(leadup, leadup_single)
  }
}
leadup <- leadup[!(is.na(leadup$calendar_month)),]
leadup_subset <- filter(leadup, diff_days <= 92)
leadup_subset$before_one_month_attrition <- leadup_subset$next_month_active == F & 
  leadup_subset$next_two_months_active == T
leadup_subset$before_two_month_attrition <- leadup_subset$next_two_months_active == F & 
  leadup_subset$next_three_months_active == T
write.csv(leadup_subset, file = "attrition_leadup_ntouched.csv")

#Graph leadups to attrition events 
leadup <- read.csv(file = "attrition_leadup_ntouched.csv")
leadup$calendar_month <- as.Date(leadup$calendar_month)
leadup <- arrange(leadup, user_pk, calendar_month)

leadup_att3 <- filter(leadup, next_three_months_active == F)
leadup_3 <- c(median(leadup_att3$month_1), median(leadup_att3$month_2), 
              median(leadup_att3$month_3), median(leadup_att3$month_4))

leadup_att2 <- filter(leadup, before_two_month_attrition == T)
leadup_2 <- c(median(leadup_att2$month_1), median(leadup_att2$month_2), 
              median(leadup_att2$month_3), median(leadup_att2$month_4))

leadup_att1 <- filter(leadup, before_one_month_attrition == T)
leadup_1 <- c(median(leadup_att1$month_1), median(leadup_att1$month_2), 
              median(leadup_att1$month_3), median(leadup_att1$month_4))

leadup_active <- filter(leadup, next_month_active == T)
leadup_comparison <- c(median(leadup_active$month_1), median(leadup_active$month_2), 
                       median(leadup_active$month_3), median(leadup_active$month_4))

leadup_data <- data.frame(cbind(rep(c(1:4), 2), c(leadup_3, leadup_comparison)))
leadup_data$X1 <- as.factor(leadup_data$X1)
month_levels <- rev(levels(leadup_data$X1))
leadup_data$att_duration <- as.factor(c(rep(">= 3 months attrition",4), rep("No attrition",4)))

g <- ggplot(leadup_data, aes(x = X1, y = X2, colour = att_duration, group = att_duration, linetype = att_duration)) +
  geom_point(shape = 15, size = 4.0, colour="peachpuff4") +
  geom_line(size = 1.5) + 
  scale_colour_brewer(palette="Set2") +
  scale_y_continuous(limits=c(0,13)) +
  scale_x_discrete(limits = month_levels) +
  xlab("Month 'X' before attrition event") +
  ylab("# cases visited") +
  theme(legend.title=element_blank())



#ncases_registered
leadup <- data.frame(matrix(ncol = 11, nrow = 1)) 
names(leadup) <- c("user_pk", "calendar_month", "previous_three_months_active", 
                   "next_month_active", "next_two_months_active", 
                   "next_three_months_active", "month_1", "month_2", "month_3", "month_4", 
                   "diff_days")
leadup$calendar_month <- as.Date(leadup$calendar_month)
leadup$user_pk <- as.numeric(leadup$user_pk)
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  single_user$calendar_month <- as.Date(single_user$calendar_month)
  for (j in 1:(nrow(single_user)-3)) {
    leadup_single <- data.frame(matrix(ncol = 11, nrow = 1)) 
    names(leadup_single) <- c("user_pk", "calendar_month", "previous_three_months_active", 
                              "next_month_active", "next_two_months_active", 
                              "next_three_months_active", "month_1", "month_2", "month_3", 
                              "month_4", "diff_days")
    leadup_single$user_pk <- as.numeric(single_user$user_pk[1])
    leadup_single$calendar_month <- as.Date(single_user$calendar_month[3+j])
    leadup_single$previous_three_months_active <- single_user$previous_three_months_active[3+j]
    leadup_single$next_month_active <- single_user$next_month_active[3+j]
    leadup_single$next_two_months_active <- single_user$next_two_months_active[3+j]
    leadup_single$next_three_months_active <- single_user$next_three_months_active[3+j]
    leadup_single$month_1 <- single_user$ncases_registered[3+j]
    leadup_single$month_2 <- single_user$ncases_registered[2+j]
    leadup_single$month_3 <- single_user$ncases_registered[1+j]
    leadup_single$month_4 <- single_user$ncases_registered[j]
    leadup_single$diff_days <- as.numeric(single_user$calendar_month[3+j] - single_user$calendar_month[j])
    leadup <- rbind(leadup, leadup_single)
  }
}
leadup <- leadup[!(is.na(leadup$calendar_month)),]
leadup_subset <- filter(leadup, diff_days <= 92)
leadup_subset$before_one_month_attrition <- leadup_subset$next_month_active == F & 
  leadup_subset$next_two_months_active == T
leadup_subset$before_two_month_attrition <- leadup_subset$next_two_months_active == F & 
  leadup_subset$next_three_months_active == T
write.csv(leadup_subset, file = "attrition_leadup_nregistered.csv")

#------------------------------------------------------------------------#
#ncases_touched
#------------------------------------------------------------------------#
users <- unique((filter(training_typical, next_month_active == F))$user_pk)
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
#Median absolute and relative values leading up to attrition period
med_leadups_1mo <- apply(as.matrix(attrition_subset), 2, median)
write.csv(attrition_subset[,1:5], file = "attrition_leadup_1mo_ntouched.csv")

users <- unique((filter(training_typical, next_two_months_active == F))$user_pk)
#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_two_months_active == F)
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
#Median absolute and relative values leading up to attrition period
med_leadups_2mos <- apply(as.matrix(attrition_subset), 2, median)
write.csv(attrition_subset[,1:5], file = "attrition_leadup_2mo_ntouched.csv")

users <- unique((filter(training_typical, next_three_months_active == F))$user_pk)
#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_three_months_active == F)
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
#Median absolute and relative values leading up to attrition period
med_leadups_3mos <- apply(as.matrix(attrition_subset), 2, median)
write.csv(attrition_subset[,1:5], file = "attrition_leadup_3mo_ntouched.csv")

#------------------------------------------------------------------------#
#ncases_registered
#------------------------------------------------------------------------#
users <- unique((filter(training_typical, next_month_active == F))$user_pk)
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
#Median absolute and relative values leading up to attrition period
med_leadups_1mo <- apply(as.matrix(attrition_subset), 2, median)
write.csv(attrition_subset[,1:5], file = "attrition_leadup_1mo_nregistered.csv")

users <- unique((filter(training_typical, next_two_months_active == F))$user_pk)
#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_two_months_active == F)
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
#Median absolute and relative values leading up to attrition period
med_leadups_2mos <- apply(as.matrix(attrition_subset), 2, median)
write.csv(attrition_subset[,1:5], file = "attrition_leadup_2mo_nregistered.csv")

users <- unique((filter(training_typical, next_three_months_active == F))$user_pk)
#Isolate all months to each attrition event for all users
#Create empty list
attrition_list <- list()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  #Create vector of all attrition positions for this user
  attrition_positions <- which(single_user$next_three_months_active == F)
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
#Median absolute and relative values leading up to attrition period
med_leadups_3mos <- apply(as.matrix(attrition_subset), 2, median)
write.csv(attrition_subset[,1:5], file = "attrition_leadup_3mo_nregistered.csv")
write.csv(data.frame(cbind(med_leadups_1mo, med_leadups_2mos, med_leadups_3mos)), 
          file = "median_nregistered_leadup.csv")