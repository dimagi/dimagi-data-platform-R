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
df[,diff:=c(NA,diff(calendar_month)),by=user_id]  
tula_data <- as.data.frame(df)
tula_data$previous_month_active <- tula_data$diff <= 31

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


#------------------------------------------------------------------------#
#Codes for tests
#------------------------------------------------------------------------#

#Number of users by calendar month
n_user <- tula_data %.% group_by(calendar_month) %.% summarise(n_users = length(unique(user_id)))
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
test_1 <- function(data) {
  
  test_1_compute <- data %.%
    group_by(domain, calendar_month) %.%
    summarise(median_indicator=median(nvisits, na.rm=TRUE), 
              mad_indicator=mad(nvisits, na.rm=TRUE))
  
  test_1_compute$med_deviation = (test_1_compute$mad_indicator/test_1_compute$median_indicator)*100
  
  #Compute CV of CVs by project
  test_1_gp_cv = group_by(test_1_compute, domain)
  test_1_compute_cv = summarise(test_1_gp_cv,
                                median_indicator = median(med_deviation, na.rm = T),
                                mad_indicator = mad(med_deviation, na.rm=T))
  test_1_compute_cv$med_deviation = (test_1_compute_cv$mad_indicator/test_1_compute_cv$median_indicator)*100
  test_1_score = median(test_1_compute_cv$med_deviation)
  #return(test_1_score)
  return(test_1_compute)
}

test_1_compute <- data.frame(test_1(tula_data))

#Calculate CV for nvisits by user_id
indicator_variation <- tula_typical %.%
  group_by(user_id) %.%
  summarise(mean_indicator = mean(nvisits, na.rm=TRUE),
            sd_indicator = sd(nvisits, na.rm=TRUE))
indicator_variation$cv <- (indicator_variation$sd_indicator / 
                             indicator_variation$mean_indicator) * 100
median(indicator_variation$cv, na.rm=TRUE)
#median cv = 63.3%
indicator_variation <- arrange(indicator_variation, cv)
#Only 32 FLWs have cv < 20

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
percentile <- function(x) ecdf(x)(x)*100

percentile_s <- paste0('percentile=percentile(', 'nvisits', ')')
raw_percentile <- tula_typical %.%
  group_by(domain, calendar_month) %.%
  s_mutate(percentile_s)

test_2_compute <- raw_percentile %.%
  group_by(domain, user_id) %.%
  summarise(
    mean_indicator=mean(percentile, na.rm=TRUE),
    sd_indicator=sd(percentile, na.rm=TRUE)
  )

test_2_compute$cv <- (test_2_compute$sd_indicator / test_2_compute$mean_indicator) * 100
test_2_score <- median(test_2_compute$cv, na.rm=TRUE)
return(test_2_score)
}

g <- ggplot(data=raw_percentile, aes(x=calendar_month, y=percentile, group = user_id)) + 
  geom_line(colour="grey", size=1.0)


#Calculate differences between percentile values for each FLW
raw_percentile <- arrange(raw_percentile, user_id, calendar_month)
df <- data.table(raw_percentile)
setkey(df,user_id)
df[,diff:=c(NA,diff(percentile)),by=user_id]  
diff_percentile <- as.data.frame(df)

myhist <- ggplot(diff_percentile, aes(x=diff)) + 
  geom_histogram(binwidth=5, colour="black", fill="lightblue")

diff_percentile_sample <- df[,diff:=c(NA,diff(sample_percentile)),by=user_id]  
diff_percentile_sample <- as.data.frame(diff_percentile_sample)

myhist <- ggplot(diff_percentile_sample, aes(x=diff)) + 
  geom_histogram(binwidth=5, colour="black", fill="lightblue")

myhist <- ggplot(diff_sample, aes(x=diff)) + 
  geom_histogram(binwidth=1, colour="black", fill="lightblue")

#Pairwise plots of consecutive months pairs
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

g <- ggplot(datset_pair_plot, aes(x=v1, y=v2)) +
  geom_point(shape=1) +
  geom_smooth(method=lm)

tula_typical %.% 
  group_by(calendar_month) %.% 
  summarise(nusers = length(unique(user_id)))

summary(tula_typical$user_id[tula_typical$calendar_month == "2012-09-01"] %in% 
          tula_typical$user_id[tula_typical$calendar_month == "2014-05-01"])

#Test 3

overall <- tula_typical %.%
  group_by(month_abbr) %.%
  summarise(visits_mean = mean(nvisits, na.rm = T),
            sd = sd(nvisits, na.rm=T),
            n = sum(!is.na(nvisits)),
            se = sd/sqrt(n))
overall$ci95 = overall$se * qt(.975, overall$n-1)

g <- ggplot(overall, aes(x=month_abbr, y=visits_mean, group = 1)) + 
  geom_line(colour="blue", size=1.0) + 
  geom_errorbar(aes(ymin=visits_mean-ci95, ymax=visits_mean+ci95), 
                width=.3, colour = "black")
