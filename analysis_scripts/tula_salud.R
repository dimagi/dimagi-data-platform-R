all_monthly_tula <- filter(all_monthly, domain == "tulasalud")

#Tula has 637 unique users
all_monthly_tula$calendar_month <- as.Date(all_monthly_tula$calendar_month)

#Exclude any users who logged > 100 visits in any month 
all_monthly_tula$visits_ge_100 <- all_monthly_tula$nvisits > 100
user_ge_100 <- all_monthly_tula %.%
  group_by(user_id) %.%
  summarise(ge_100 = sum(visits_ge_100))
user_le_100 <- filter(user_ge_100, ge_100 == 0)
#513 users have only <= 100 visits per month
tula_typical <- 
  all_monthly_tula[all_monthly_tula$user_id %in% user_le_100$user_id, ]

#Exclude users who started in June 2014 onwards
user_start_month <- tula_typical %.%
  group_by(user_id) %.%
  summarise(first_month = min(calendar_month))
user_start_month <- user_start_month[user_start_month$first_month <= "2014-05-01",]
#118 users have start months before June 2014
tula_typical <- 
  tula_typical[tula_typical$user_id %in% user_start_month$user_id, ]

#Exclude users with < 4 months on CC
month_count <- tula_typical %.%
  group_by(domain, user_id) %.%
  summarise(months_on_cc = length(unique(calendar_month)))
month_count <- filter(month_count, months_on_cc >= 4)
#91 users have >= 4 months on CC
tula_typical <- 
  tula_typical[tula_typical$user_id %in% month_count$user_id, ]

#Exclude visits prior to 2012-09-01 
#(because they have very few number of users)
tula_typical <- filter(tula_typical, calendar_month >= "2012-09-01")

#Add sample_percentile variable
tula_typical$sample_percentile <- 
  sample(1:100, nrow(tula_typical), replace=T)

#Calculate differences between month_index to calculate next_month_active
#previous_month_active
tula_typical <- arrange(tula_typical, domain_numeric, user_id, 
                          calendar_month)
df <- data.table(tula_typical)
#Can we setkey by domain and user_id since some user_ids might be the same?
setkey(df,user_id)
df[,diff_days:=c(NA,diff(calendar_month)),by=user_id]
df[,diff_nvisits:=c(NA,diff(nvisits)),by=user_id]
tula_data <- as.data.frame(df)
tula_data$previous_month_active <- tula_data$diff_days <= 31

users <- unique(tula_data$user_id)
next_month_active <- c()
for (i in users) {
  single_user <- tula_data[tula_data$user_id == i,]
  next_active <- c()
  next_active <- append(single_user$previous_month_active[-1], F)
  next_month_active <- append(next_month_active, next_active)
}
tula_data$next_month_active <- next_month_active

#If calendar_month = 8/1/14 then next_month_active = NA
#because we don't know if the user will be active in the following month
is.na(tula_data$next_month_active) <- tula_data$calendar_month == "2014-08-01"

# % difference in nvisits for each user for truly consectutive months 
per_diff_nvisits <- c()
for (i in users) {
  single_user <- tula_data[tula_data$user_id == i,]
  prev_visits <- c()
  prev_visits <- append(NA, single_user$nvisits)
  prev_visits <- prev_visits[-length(prev_visits)]
  per_diff <- c()
  per_diff <- (single_user$diff_nvisits/prev_visits)*100
  per_diff_nvisits <- append(per_diff_nvisits, per_diff)
}
tula_data$per_diff_nvisits <- per_diff_nvisits
#------------------------------------------------------------------------#
#Codes for tests
#------------------------------------------------------------------------#

#Number of users by calendar month
n_user <- tula_data %.% 
  group_by(calendar_month) %.% 
  summarise(n_users = length(unique(user_id)))

g <- ggplot(n_user, aes(x=calendar_month, y=n_users)) +
  geom_point(size = 6, shape = 19, alpha = 0.5, colour = "darkblue", 
             fill = "lightblue") +
  geom_line(colour = "darkblue") + 
  scale_size_area() + 
  scale_y_continuous(limits=c(0,100)) +
  xlab("Calendar month") +
  ylab("# unique users/month") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                 face="bold")) + 
  ggtitle("Number of users by calendar month") +
  theme(plot.title = element_text(size=14, face="bold"))

pdf("plots.pdf")
plot(g)
dev.off()

#Calculate median absolute deviation (MAD) per calendar month
#Use this to calculate rCV for the domain per calendar month

test_1 <- function(data) {
  
  test_1_compute <- data %.%
    group_by(domain, calendar_month) %.%
    summarise(median_indicator=median(nvisits, na.rm=TRUE), 
              mad_indicator=mad(nvisits, na.rm=TRUE))
  
  test_1_compute$rcv = (test_1_compute$mad_indicator/test_1_compute$median_indicator)*100
  
  #Compute CV of CVs by project
  test_1_gp_cv = group_by(test_1_compute, domain)
  test_1_compute_cv = summarise(test_1_gp_cv,
                                median_indicator = median(rcv, na.rm = T),
                                mad_indicator = mad(rcv, na.rm=T))
  test_1_compute_cv$rcv = (test_1_compute_cv$mad_indicator/test_1_compute_cv$median_indicator)*100
  test_1_score = median(test_1_compute_cv$rcv)
  #return(test_1_score)
  return(test_1_compute)
}

test_1_compute <- data.frame(test_1(tula_data))

g <- ggplot(test_1_compute, aes(x=calendar_month, y=median_indicator, group = 1)) + 
  geom_line(colour="blue", size=1.0) + 
  geom_errorbar(aes(ymin=median_indicator-mad_indicator, ymax=median_indicator+mad_indicator), 
                width=.3, colour = "black")

g <- ggplot(test_1_compute, aes(x=calendar_month, y=rcv)) +
  geom_point(size = 6, shape = 19, alpha = 0.5, colour = "red", 
             fill = "pink") +
  geom_line(colour = "red") + 
  scale_size_area() + 
  scale_y_continuous(limits=c(0,100)) +
  xlab("Calendar month") +
  ylab("rCV(%)") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                 face="bold")) + 
  ggtitle("rCV(%) by calendar month") +
  theme(plot.title = element_text(size=14, face="bold"))
  
tula_median_per_change <- tula_data[tula_data$previous_month_active == T,] %.% 
  group_by(calendar_month) %.%
  summarise(med_domain_per_change = median(per_diff_nvisits, na.rm = T))

#Merge med_domain_per_change to tula_data
detach("package:data.table")
tula_data <- merge(tula_data, tula_median_per_change, by = "calendar_month", 
                   all.x = T)

g <- ggplot(data=tula_median_per_change, aes(x=calendar_month, y=med_domain_per_change)) + 
  geom_line(colour="black", size=1.0) + 
  geom_point(colour="red", size=3, shape=21, fill="red")

test = tula_data[tula_data$previous_month_active == T,]

g <- ggplot(test, aes(x=med_domain_per_change, y=per_diff_nvisits)) +
  geom_point(shape=1) +
  scale_y_continuous(limits=c(-100,100)) +
  geom_smooth(method=lm)
cor(tula_data$med_domain_per_change, 
    tula_data$per_diff_nvisits, use = "complete.obs")

#Rehape all_monthly from long format to wide
#This creates only one row per user with columns for each calendar month
users_long <- select(tula_typical, domain, user_id, nvisits, calendar_month)
users_wide <- reshape(users_long,
                      timevar = "calendar_month",
                      idvar = c("domain", "user_id"),
                      direction = "wide")

g <- ggplot(data=tula_typical, aes(x=calendar_month, y=nvisits, group = user_id)) + 
  geom_line(colour="grey", size=1.0)

users_long <- select(raw_percentile, domain, user_id, percentile, calendar_month)
users_wide <- reshape(users_long,
                      timevar = "calendar_month",
                      idvar = c("domain", "user_id"),
                      direction = "wide")

# Number of users by calendar_month
users_month_tula <- tula_typical %.% 
  group_by(domain, calendar_month) %.% 
  summarise(nusers = length(unique(user_id)))

g <- ggplot(data=users_month_tula, aes(x=calendar_month, y=nusers)) + 
  geom_line(colour="black", size=1.0) + 
  geom_point(colour="red", size=3, shape=21, fill="red")

users_wide <- reshape(users_month_tula,
                      timevar = "calendar_month",
                      idvar = c("domain"),
                      direction = "wide")

users_wide <- users_wide[,order(names(users_wide))]
write.csv(users_wide, file = "tula_nusers_wide.csv")

#TEST 2
#Manual
percentile <- function(x) rank(x, ties.method = "average")/length(x)*100
percentile_s <- paste0('percentile=percentile(', 'nvisits', ')')

raw_percentile <- tula_data %.%
  group_by(domain, calendar_month) %.%
  s_mutate(percentile_s)

test_2_compute <- raw_percentile %.%
  group_by(domain, user_id) %.%
  summarise(
    median_indicator=median(percentile, na.rm=TRUE),
    mad_indicator=mad(percentile, na.rm=TRUE)
  )

test_2_compute$rcv <- (test_2_compute$mad_indicator / test_2_compute$median_indicator) * 100


g <- ggplot(data=tula_data, aes(x=calendar_month, y=nvisits, group = user_id)) + 
  geom_line(colour="grey", size=1.0)

g <- ggplot(data=raw_percentile, aes(x=calendar_month, y=percentile, group = user_id)) + 
  geom_line(colour="grey", size=1.0)


#Calculate differences between percentile values for each FLW
raw_percentile <- arrange(raw_percentile, user_id, calendar_month)
df <- data.table(raw_percentile)
setkey(df,user_id)
df[,diff:=c(NA,diff(percentile)),by=user_id]  
diff_percentile <- as.data.frame(df)
diff_percentile <- diff_percentile[diff_percentile$previous_month_active == T,]

myhist <- ggplot(diff_percentile, aes(x=diff)) + 
  geom_histogram(binwidth=5, colour="black", fill="lightblue")

diff_percentile_sample <- df[,diff:=c(NA,diff(sample_percentile)),by=user_id]  
diff_percentile_sample <- as.data.frame(diff_percentile_sample)
diff_percentile_sample <- diff_percentile_sample[diff_percentile_sample$previous_month_active == T,]

myhist <- ggplot(diff_percentile_sample, aes(x=diff)) + 
  geom_histogram(binwidth=5, colour="black", fill="lightblue")

myhist <- ggplot(diff_sample, aes(x=diff)) + 
  geom_histogram(binwidth=1, colour="black", fill="lightblue")

#Pairwise plots of true consecutive months pairs
prev_nvisits <- c()
prev_percentile <- c()
prev_percentile_sample <- c()
for (i in users) {
  single_user <- raw_percentile[raw_percentile$user_id == i,]
  
  prev_vis <- c()
  prev_vis <- append(NA, single_user$nvisits)
  prev_vis <- prev_vis[-length(prev_vis)]
  prev_nvisits <- append(prev_nvisits, prev_vis)
  
  prev <- c()
  prev <- append(NA, single_user$percentile)
  prev <- prev[-length(prev)]
  prev_percentile <- append(prev_percentile, prev)
  
  prev_sample <- c()
  prev_sample <- append(NA, single_user$sample_percentile)
  prev_sample <- prev_sample[-length(prev_sample)]
  prev_percentile_sample <- append(prev_percentile_sample, prev_sample)
}
raw_percentile$prev_percentile <- prev_percentile
raw_percentile$prev_percentile_sample <- prev_percentile_sample
raw_percentile$prev_nvisits <- prev_nvisits
raw_percentile <- raw_percentile[raw_percentile$previous_month_active == T,]

g <- ggplot(raw_percentile, aes(x=sample_percentile, y=prev_percentile_sample)) +
  geom_point(shape=1) +
  geom_smooth(method=lm)
cor(raw_percentile$sample_percentile, raw_percentile$prev_percentile_sample, use = "complete.obs")

#Older code not excluding skipped months
v1 = NA
v2 = NA
user_id = NA
grp <- unique(raw_percentile$user_id)
raw_percentile <- arrange(raw_percentile, user_id, calendar_month)
for(i in grp){
  rownums = which(raw_percentile$user_id==i)
  v1 = append(v1,raw_percentile$percentile[rownums[1:(length(rownums)-1)]])
  v2 = append(v2,raw_percentile$percentile[rownums[2:(length(rownums))]])
  user_id = append(user_id,rep(i,times=(length(rownums)-1)))
}

datset_pair_plot = data.frame(user_id,v1,v2)
datset_pair_plot <- datset_pair_plot[-1,]

tula_typical %.% 
  group_by(calendar_month) %.% 
  summarise(nusers = length(unique(user_id)))

summary(tula_typical$user_id[tula_typical$calendar_month == "2012-09-01"] %in% 
          tula_typical$user_id[tula_typical$calendar_month == "2014-05-01"])

#Test 3

overall <- tula_data %.%
  group_by(month_abbr) %.%
  summarise(visits_median = median(nvisits, na.rm = T),
            visits_mad = mad(nvisits, na.rm = T))

g <- ggplot(overall, aes(x=month_abbr, y=visits_median, group = 1)) + 
  geom_line(colour="blue", size=1.0) + 
  geom_errorbar(aes(ymin=visits_median-visits_mad, ymax=visits_median+visits_mad), 
                width=.3, colour = "black") +
  scale_y_continuous(limits=c(-5,100))

#Test 4
attrition <- tula_data %.% 
  group_by(user_id) %.% 
  summarise(attrition = sum(!next_month_active, na.rm = T))

test <- data.frame(table(attrition$attrition))

g <- ggplot(test, aes(x=Var1, y=Freq, group = 1)) +
  geom_point(size = 6, shape = 19, alpha = 0.5, colour = "red", 
             fill = "pink") +
  geom_line(colour = "red") + 
  scale_size_area() + 
  scale_y_continuous(limits=c(0,50)) +
  xlab("# attrition events") +
  ylab("# of unique FLWs") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                 face="bold")) + 
  ggtitle("Number of unique FLWs by number of attrition events") +
  theme(plot.title = element_text(size=14, face="bold"))

#Calculate time to final attrition event for users who quit before 

