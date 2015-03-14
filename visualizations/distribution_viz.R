##############################
##distribution visualization##
##############################

# create a single line graph from a set of user-months and an activity indicator showing distribution of levels   
  # For each indicator
  # the x-axis should be the value of the indicator (percent of active days)
  # the y-axis should be the percent of user-months that have that value
  # multiple graphs will be generated on different sets of user-months 
  # graph naming format: p_n (n: the nth graph)

data <- tbl_df(read.csv("blog_data_2_2_15.csv", stringsAsFactors = FALSE))
data$adp <- round(data$active_day_percent) # rounding would make it a discrete variable

library(plyr)
library(dplyr)
library(ggplot2)
library(Cairo)
library(zoo)
library(RColorBrewer)
library(lattice)
library(grid)
library(gridExtra)


# distribution visualization of performance indicators

# Custom Quarter entirely by domain
d_1 = select(data, user_pk, domain_numeric, calendar_month, active_days, month_index)
d_1 = arrange(d_1, calendar_month, domain_numeric)
# create month_seq by domain
d_1 = d_1 %>%
  group_by(domain_numeric) %>%
  mutate(month_seq_by_domain = rep(1:length(table(calendar_month)),
                                   table(calendar_month))
  )	
# create custom quarter by month_index 
bins = c(1+(
  3*(0:ceiling(max(d_1$month_seq_by_domain)/3))
))

labs = paste("Q_", seq(length(bins)-1), sep = "")

d_1 = d_1 %>% 
  group_by(domain_numeric) %>%
  mutate(QTR_by_domain = cut(month_seq_by_domain,
                             breaks = bins,
                             labels = labs,
                             right = FALSE)
  )

pdf(7, 14, file="custom qtr by domain.pdf")
p_1 = densityplot(~active_days|QTR_by_domain, data=d_1,
                  panel=function(x,...){
                    panel.densityplot(x,...)
                    lower.v <- quantile(x,probs=0.25)
                    median.v <- median(x) 
                    upper.v <- quantile(x,probs=0.75)
                    mean.v <- mean(x)
                    
                    panel.abline(v=median.v, col.line="#003d71", lwd=1.2)  # 50 percentile
                    #                   panel.abline(v=lower.v, col.line="#f58220") # 25 percentile
                    #                   panel.abline(v=upper.v, col.line="#f58220") # 75 percentile
                    panel.abline(v=mean.v, col.line="#003d71", lwd=1.2, lty=2) # mean
                    
                    dens <- density(x)                   
                    min.v <- min(dens$x)
                    max.v <- max(dens$x)
                    panel.polygon(c(upper.v, dens$x[dens$x > upper.v & dens$x < max.v], max.v), 
                                  c(0, dens$y[dens$x> upper.v & dens$x < max.v], 0),
                                  col="#f58220")
                    panel.polygon(c(min.v, dens$x[dens$x > min.v & dens$x < lower.v], lower.v),
                                  c(0, dens$y[dens$x > min.v & dens$x < lower.v], 0),
                                  col="#f58220")
                  },
                  scales=list(x=list(at=c(1,2,3,4,5,6,7,8,9,10,20,30))),
                  xlim=range(d_1$active_days), # reversing the order of x-axis values
                  ylim=c(0,0.5),
                  xlab="Active days per month",
                  ylab="Density",
                  #                 aspect=0.1,
                  layout=c(1,length(bins)-1),
                  index.cond=list((length(bins)-1):1),
                  strip=strip.custom(bg="#ffffff", par.strip.text=list(font=0.5,col="#413f3f")),
                  col="#413f3f",
                  plot.points=FALSE)
print(p_1)
dev.off()

# get a subset of users that have no dropout(gap) months from their first active month till 80% of domain active months
d_2 = d_1 %>%
  group_by(domain_numeric) %>%
  summarise(max_months = max(month_seq_by_domain))
d_2$pct_80 = floor(d_2$max_months*0.8)  # for each domain, calculate the first 80 percent months

d_2 %>%
  group_by(domain_numeric) %>%
  filter() 

# for each user, get the distance between two active months 
d_1 = arrange(d_1, domain_numeric, user_pk, month_seq_by_domain)
d_1 = d_1%>%
  group_by(domain_numeric, user_pk) %>%
  mutate(diff_month = c(NA, diff(month_seq_by_domain)))	

# get the last row per user, return users that have lived through the first 80 percent of domain active months
d_2 = left_join(d_1, d_2)	
d_2 = arrange(d_2, domain_numeric, user_pk, calendar_month)
d_2_user = d_2 %>%
  group_by(domain_numeric, user_pk) %>%
  do(tail(.,n=1)) %>% 
  filter(., month_seq_by_domain >= pct_80)
d_2 = filter(d_2, user_pk %in% d_2_user$user_pk)
# filter out users who lived through but have dropouts during the first 80 percent 
d_2_80 = filter(d_2, month_seq_by_domain <= pct_80)  # for users kept in d_2_user, truncate their activity to pct_80
d_2_80 = d_2_80 %>%
  group_by(domain_numeric, user_pk) %>%
  filter(., n()==max(month_seq_by_domain)) # excluding all users that have gaps during the first 80 percent of domain active months

d_3 = filter(d_1, user_pk %in% d_2_user$user_pk) # this returns all user-months that live through first 80% of domain active months with no dropouts

# get the subset of users that have contribute to 14 quarters
d_3_subset_user = d_3 %>%
  group_by(domain_numeric, user_pk) %>%
  summarise(tot_qtr = length(levels(factor(QTR_by_domain)))) %>% # this returns total active quarters per user
  filter(., tot_qtr >= 6)

# only keep data till QTR_6 for visual
d_4 = filter(d_3, user_pk %in% d_3_subset_user$user_pk)
d_4_subset = filter(d_4, month_seq_by_domain <= 18)
d_4_subset$QTR_by_domain = factor(d_4_subset$QTR_by_domain) # drop unused levels

pdf(7, 14, file="no dropout in 6 quarters.pdf")
p_qtr_6_by_domain = densityplot(~active_days|QTR_by_domain, data=d_4_subset,
                                panel=function(x,...){
                                  panel.densityplot(x,...)
                                  lower.v <- quantile(x,probs=0.25)
                                  median.v <- median(x) 
                                  upper.v <- quantile(x,probs=0.75)
                                  mean.v <- mean(x)
                                  
                                  panel.abline(v=median.v, col.line="#003d71", lwd=1.2)  # 50 percentile
                                  #                   panel.abline(v=lower.v, col.line="#f58220") # 25 percentile
                                  #                   panel.abline(v=upper.v, col.line="#f58220") # 75 percentile
                                  panel.abline(v=mean.v, col.line="#003d71", lwd=1.2, lty=2) # mean
                                  
                                  dens <- density(x)                   
                                  min.v <- min(dens$x)
                                  max.v <- max(dens$x)
                                  panel.polygon(c(upper.v, dens$x[dens$x > upper.v & dens$x < max.v], max.v), 
                                                c(0, dens$y[dens$x> upper.v & dens$x < max.v], 0),
                                                col="#f58220")
                                  panel.polygon(c(min.v, dens$x[dens$x > min.v & dens$x < lower.v], lower.v),
                                                c(0, dens$y[dens$x > min.v & dens$x < lower.v], 0),
                                                col="#f58220")
                                },
                                # scales=list(x=list(at=c(1,2,3,4,5,6,7,8,9,10,20,30))),
                                xlim=range(d_4_subset$active_days), # reversing the order of x-axis values
                                ylim=c(0,0.5),
                                xlab="Active days per month",
                                ylab="Density",
                                #                 aspect=0.1,
                                layout=c(1,length(levels(d_4_subset$QTR_by_domain))),
                                index.cond=list(length(levels(d_4_subset$QTR_by_domain)):1),
                                strip=strip.custom(bg="#ffffff", par.strip.text=list(font=0.5,col="#413f3f")),
                                col="#413f3f",
                                plot.points=FALSE)
print(p_qtr_6_by_domain)
dev.off()



# custom quarter by user
pdf(7, 14, file="custom qtr by user (no dropout).pdf")
d_2$QTR = factor(d_2$QTR)
p_2_by_user = densityplot(~active_days|QTR, data=d_2,
                          panel=function(x,...){
                            panel.densityplot(x,...)
                            lower.v <- quantile(x,probs=0.25)
                            median.v <- median(x) 
                            upper.v <- quantile(x,probs=0.75)
                            mean.v <- mean(x)
                            
                            panel.abline(v=median.v, col.line="#003d71", lwd=1.2)  # 50 percentile
                            #                   panel.abline(v=lower.v, col.line="#f58220") # 25 percentile
                            #                   panel.abline(v=upper.v, col.line="#f58220") # 75 percentile
                            panel.abline(v=mean.v, col.line="#003d71", lwd=1.2, lty=2) # mean
                            
                            dens <- density(x)                   
                            min.v <- min(dens$x)
                            max.v <- max(dens$x)
                            panel.polygon(c(upper.v, dens$x[dens$x > upper.v & dens$x < max.v], max.v), 
                                          c(0, dens$y[dens$x> upper.v & dens$x < max.v], 0),
                                          col="#f58220")
                            panel.polygon(c(min.v, dens$x[dens$x > min.v & dens$x < lower.v], lower.v),
                                          c(0, dens$y[dens$x > min.v & dens$x < lower.v], 0),
                                          col="#f58220")
                          },
                          scales=list(x=list(at=c(1,2,3,4,5,6,7,8,9,10,20,30))),
                          xlim=range(d_2$active_days), # reversing the order of x-axis values
                          ylim=c(0,0.5),
                          xlab="Active days per month",
                          ylab="Density",
                          #                 aspect=0.1,
                          layout=c(1,length(levels(d_2$QTR))),
                          index.cond=list(length(levels(d_2$QTR)):1),
                          strip=strip.custom(bg="#ffffff", par.strip.text=list(font=0.5,col="#413f3f")),
                          col="#413f3f",
                          plot.points=FALSE)

print(p_2_by_user)
dev.off()







# Distribution Graphs 1: Kernel Density Plot for all user-months
  # construct data table for graph 1
adp_groups <- data %>% group_by(adp) # order data table by percent of active days
x <- unique(adp_groups$adp) # extract unique percent of active days 
y <- group_size(adp_groups) # calculate the occurrence of each value in percent of active days
data_adp1 <- tbl_df(as.data.frame(cbind(x,y))) # combine percentage of each value occurring and unique values into a new data table
data_adp1 <- arrange(data_adp1, x) # reorder the data table by unique values in percent of active days
data_adp1$y_prob <- round(y/sum(data_adp1$y),digits=6) # calculate the percentage of each value occurring in the whole data set
  # run plot
png(600,600,file="pad_all_domains.png")
d1 = density(data$active_days)
p1 = plot(d1, main="Distribution of active days per month (Kernel density estimation)")
dev.off()





# Distribution Graph 2: Probability Distribution by Custom Quarter
  # Create custom quarters entirely by domain
d_1 = select(data, user_pk, domain_numeric, calendar_month, active_days, month_index)


  # Create custom quarters entirely by user
d_2 = select(data, user_pk, domain_numeric, calendar_month, active_days)
d_2 %>%
  group_by(domain_numeric, user_pk) %>%
  mutate(month_seq_by_user = c(1:length(calendar_month)))


labs = paste("QTR_by_use ",seq(17),sep="") # there are 17 quarters per n_distinct(calendar_month) by user
d_2$QTR = cut(d_2$month_seq_by_user, breaks=3*(0:17), labels=labs) # adding labels
d_2$QTR = factor(d_2$QTR, levels = levels(d_2$QTR))


p_2 = densityplot(~active_days|QTR, data=d_2,
                 panel=function(x,...){
                   panel.densityplot(x,...)
                   lower.v <- quantile(x,probs=0.25)
                   median.v <- median(x) 
                   upper.v <- quantile(x,probs=0.75)
                   mean.v <- mean(x)
                   
                   panel.abline(v=median.v, col.line="red")  # 50 percentile
#                   panel.abline(v=lower.v, col.line="#f58220") # 25 percentile
#                   panel.abline(v=upper.v, col.line="#f58220") # 75 percentile
                   panel.abline(v=mean.v, col.line="darkorange", lty=2) # mean

                   dens <- density(x)                   
                   min.v <- min(dens$x)
                   max.v <- max(dens$x)
                   panel.polygon(c(upper.v, dens$x[dens$x > upper.v & dens$x < max.v], max.v), 
                                 c(0, dens$y[dens$x> upper.v & dens$x < max.v], 0),
                                 col="#f58220")
                   panel.polygon(c(min.v, dens$x[dens$x > min.v & dens$x < lower.v], lower.v),
                                 c(0, dens$y[dens$x > min.v & dens$x < lower.v], 0),
                                 col="#f58220")
                 },
                 scales=list(x=list(at=c(1,2,3,4,5,6,7,8,9,10,20,30))),
                 xlim=range(d_2$active_days), # reversing the order of x-axis values
                 xlab="Active days per month",
                 ylab="Density",
#                 aspect=0.1,
                 layout=c(1,17),
                 index.cond=list(17:1),
                 strip=strip.custom(bg="#003d71", par.strip.text=list(font=1,col="white")),
                 col="#413f3f",
                 plot.points=FALSE)



# run plot on a subset of domains that have at least one user active for 10 QTRs
max_qtr = d_2 %>%
  group_by(domain_numeric) %>%
  summarise(max(as.numeric(QTR)))
colnames(max_qtr) = c("domain_numeric", "max_q")
dm = filter(max_qtr, max_q >= 10)$domain_numeric
d_3 = filter(d_2, domain_numeric %in% dm) 


pdf("distribution_final.pdf",
    width=6,
    height=12)

print(densityplot(~active_days|QTR, data=d_3,
                  panel=function(x,...){
                    panel.densityplot(x,...)
                    lower.v <- quantile(x,probs=0.25)
                    median.v <- median(x) 
                    upper.v <- quantile(x,probs=0.75)
                    mean.v <- mean(x)
                    
                    panel.abline(v=median.v, col.line="#f58220", lwd=2)  # 50 percentile
                    #                   panel.abline(v=lower.v, col.line="#f58220") # 25 percentile
                    #                   panel.abline(v=upper.v, col.line="#f58220") # 75 percentile
                    panel.abline(v=mean.v, col.line="#f58220", lty=2, lwd=2) # mean
                    
                    dens <- density(x)                   
                    min.v <- min(dens$x)
                    max.v <- max(dens$x)
                    panel.polygon(c(upper.v, dens$x[dens$x > upper.v & dens$x < max.v], max.v), 
                                  c(0, dens$y[dens$x> upper.v & dens$x < max.v], 0),
                                  col="#f58220")
                    panel.polygon(c(min.v, dens$x[dens$x > min.v & dens$x < lower.v], lower.v),
                                  c(0, dens$y[dens$x > min.v & dens$x < lower.v], 0),
                                  col="#f58220")
                  },
                  scales=list(x=list(at=c(1,2,3,4,5,6,7,8,9,10,20,30))),
                  xlim=range(d_3$active_days), # reversing the order of x-axis values
                  xlab="Active days per month",
                  ylab="Density",
                  #                 aspect=0.1,
                  layout=c(1,17),
                  index.cond=list(17:1),
                  strip=strip.custom(bg="#003d71", par.strip.text=list(font=1,col="white")),
                  col="#413f3f",
                  plot.points=FALSE))

dev.off()

round(nrow(d_3)/nrow(data),digits=2) # contribution of these 7 domains to all_domain data is 34%


# d_4: for each domain, exclude users that were inactive for the first 80% of the N months, 
# namely to exclude users whose month_start > 08*calendar_month_index

# for each domain, generate continuous id for each unique calendar month 
d_4 = select(data, user_pk, domain_numeric, calendar_month, active_days, QTR)
d_4 = arrange(d_4, domain_numeric, calendar_month)
d_4 = 
  d_4 %>%
  group_by(domain_numeric) %>%
  mutate(monthCode = rep(seq(n_distinct(calendar_month
                            )),as.numeric(table(calendar_month))))

d_4 = arrange(d_4, domain_numeric, user_pk)

tot_active_months = 
  d_4 %>%
  group_by(domain_numeric) %>%
  summarise(monthMax = max(monthCode))


tot_active_months$month80 = round(tot_active_months$monthMax*0.8)

# for each user, left join first user-month with month80
d_4 = arrange(d_4, domain_numeric, user_pk, calendar_month)
d_4_first = d_4 %>%
  group_by(domain_numeric, user_pk) %>%
  filter(calendar_month == min(calendar_month))


d_4_first_join = left_join(d_4_first, tot_active_months)
# compare the first user-month with the 80%-month per domain
d_4_first_join$later_than_80 = ifelse(d_4_first_join$monthCode >= d_4_first_join$month80, "yes", "no")

# if the first user-month is later than the 80% cutoff, drop it out
d_4_user_subset = filter(d_4_first_join, later_than_80 == "no")$user_pk

# extract data for the subset of user
d_4_final = select(filter(data, user_pk %in% d_4_user_subset), 
                   user_pk, domain_numeric, calendar_month, active_days, QTR)


# check out which domains are dropped in the process 
dropped = 
  as.numeric(
  unique(d_4$domain_numeric) %in% d_4_final$domain_numeric)
dropped_dm = 
  unique(d_4$domain_numeric)[which(dropped == 0)]


# print out the table of user distribution per domain (see if there are skyrocketing domains)
d_4_final_user = 
  d_4_final %.%
  group_by(domain_numeric) %.%
  summarise(numUser = n_distinct(user_pk))

# barplot users per domain
d_4_final_user = arrange(d_4_final_user, -numUser)
barplot(d_4_final_user$numUser)

d_4_final_user$userBin = ifelse(d_4_final_user$numUser < 5, "1",
                                ifelse(d_4_final_user$numUser >= 5 & d_4_final_user$numUser < 25, "2",
                                       ifelse(d_4_final_user$numUser >= 25 & d_4_final_user$numUser < 50, "3",
                                              ifelse(d_4_final_user$numUser >= 50 & d_4_final_user$numUser < 100, "4", "5"
                                                    ))))

barplot(as.numeric(d_4_final_user$userBin))
table(as.numeric(d_4_final_user$userBin))

d_4_final = left_join(d_4_final, d_4_final_user)
d_4_final$QTR = factor(d_4_final$QTR)

print(densityplot(~active_days|QTR, data=filter(d_4_final, userBin == 3),
                  panel=function(x,...){
                    panel.densityplot(x,...)
                    lower.v <- quantile(x,probs=0.25)
                    median.v <- median(x) 
                    upper.v <- quantile(x,probs=0.75)
                    mean.v <- mean(x)
                    
                    panel.abline(v=median.v, col.line="#f58220", lwd=2)  # 50 percentile
                    #                   panel.abline(v=lower.v, col.line="#f58220") # 25 percentile
                    #                   panel.abline(v=upper.v, col.line="#f58220") # 75 percentile
                    panel.abline(v=mean.v, col.line="#f58220", lty=2, lwd=2) # mean
                    
                    dens <- density(x)                   
                    min.v <- min(dens$x)
                    max.v <- max(dens$x)
                    panel.polygon(c(upper.v, dens$x[dens$x > upper.v & dens$x < max.v], max.v), 
                                  c(0, dens$y[dens$x> upper.v & dens$x < max.v], 0),
                                  col="#f58220")
                    panel.polygon(c(min.v, dens$x[dens$x > min.v & dens$x < lower.v], lower.v),
                                  c(0, dens$y[dens$x > min.v & dens$x < lower.v], 0),
                                  col="#f58220")
                  },
                  scales=list(x=list(at=c(1,2,3,4,5,6,7,8,9,10,20,30))),
                  between=list(y=0.5), # introduce a gap between rows of vertical panels
                  par.strip.text=list(cex=0.7), # size down the text in the strip above each panel
                  par.settings=list(axis.text=list(cex=0.7)), # control size of the text on tick lablels
                  xlim=range(d_4_final$active_days), # reversing the order of x-axis values
                  xlab="Active days per month",
                  ylab="Density",
                  
                  # custom arrangement of panel layout  
                  aspect=1,
                  layout=c(1,17), # single column, 17 rows, one for each quarter
                  index.cond=list(17:1), # panels should be ordered from the top to the bottom
                  strip=strip.custom(bg="#003d71", par.strip.text=list(font=1,col="white")),
                  col="#413f3f",
                  plot.points=FALSE))





# summary data table as a supplement 
p_data = group_by(p_data, QTR)
viz_sum_table = summarise(p_data, round(mean(active_days)), round(median(active_days)), round(quantile(active_days,probs=0.25)), round(quantile(active_days,probs=0.75)))

# create a dimagi theme



# graph 2: subset data from 10 biggest domains (in terms of user-months)
tum <- tally(group_by(data,domain_numeric)) # for each domain, count total user-month 
colnames(tum) <- c("domain_numeric","user_months") # renaming variables
top_10_dm <- tum %>%
  arrange(desc(user_months)) %>%
  top_n(10) # identify the qualifying 10 domains
top_10 <- filter(data, domain_numeric %in% top_10_dm$domain_numeric) # subset data 
top_10$calendar_month <- as.Date(top_10$calendar_month)

# generate a sequence of active months on each domain
top_10 = top_10 %>%
  group_by(domain_numeric,user_pk) %>%
  mutate(month_seq_by_user = seq(n())) # for each user this calculates the total active month 

# for each user on each domain, categorize active month by quarter (not calendar quarter)
top_10$YR = ifelse(top_10$month_seq_by_user <= 12, "Y1","Y2")
top_10_Y1 = filter(top_10, YR == "Y1")
top_10_Y1$QR = sapply(top_10_Y1$month_seq_by_user, function(x){
  if(x <= 3) return("Q1")
  if(x > 3 & x <= 6) return("Q2")
  if(x > 6 & x <= 9) return("Q3")
  else return("Q4")
})


# Distribution graph 2 (alternative: use sm.density.compare)
d1 = densityplot(~active_days,
                data=top_10_Y1,
                groups=QR,
                xlab="Active days per month",
                main="Top 10 domains",
                from=0,to=28,
                auto.key=TRUE,
                plot.points=FALSE)


d2 = densityplot(~active_days|QR, data=top_10_Y1,
            panel=function(x,...){
              panel.densityplot(x,...)
              lowerp.values <- quantile(x,probs=0.25)
              median.values <- median(x) 
              upperp.values <- quantile(x,probs=0.75)
              mean.values <- mean(x)
              panel.abline(v=median.values, col.line="red") 
              panel.abline(v=lowerp.values, col.line="yellow")
              panel.abline(v=upperp.values, col.line="green")
              panel.abline(v=mean.values, col.line="pink", lty=2)
            },
            xlim=range(top_10_Y1$active_days), # reversing the order of x-axis values
            layout=c(1,4),
            index.cond=list(4:1),
            plot.points=FALSE)

# create plots separately for each domain

dm_list = split(top_10_Y1, top_10_Y1$domain_numeric)
dm_1 = densityplot(~active_days|QR, data=dm_list[[1]],
                   panel=function(x,...){
                     panel.densityplot(x,...)
                     lowerp.values <- quantile(x,probs=0.25)
                     median.values <- median(x) 
                     upperp.values <- quantile(x,probs=0.75)
                     mean.values <- mean(x)
                     panel.abline(v=median.values, col.line="red") 
                     panel.abline(v=lowerp.values, col.line="yellow")
                     panel.abline(v=upperp.values, col.line="green")
                     panel.abline(v=mean.values, col.line="pink", lty=2)
                   },
                   xlim=range(dm_list[[1]]$active_days), # reversing the order of x-axis values
                   layout=c(1,4),
                   index.cond=list(4:1),
                   plot.points=FALSE)

densPlot = function(dm_data){
  densityplot(~active_days|QR, data=dm_data,
              panel=function(x,...){
                panel.densityplot(x,...)
                lowerp.values <- quantile(x,probs=0.25)
                median.values <- median(x) 
                upperp.values <- quantile(x,probs=0.75)
                mean.values <- mean(x)
                panel.abline(v=median.values, col.line="red") 
                panel.abline(v=lowerp.values, col.line="yellow")
                panel.abline(v=upperp.values, col.line="green")
                panel.abline(v=mean.values, col.line="pink", lty=2)
              },
              xlim=range(dm_data$active_days), # reversing the order of x-axis values
              layout=c(1,4),
              index.cond=list(4:1),
              plot.points=FALSE)
}

plots_separate = lapply(names(dm_list), function(x) densPlot(dm_list[[x]]))
png("density_plot_per_domain.png")
do.call(grid.arrange, plots_separate)
dev.off()




toList = quantile(top_10_Y1$active_days, probs=c(0,0.25,0.5,0.75,1))

lattice.theme <- trellis.par.get()
col <- lattice.theme$superpose.symbol$col

plots = lapply(1:4,function(.x){
  densityplot(~active_days,
              data=top_10_Y1,
              group=QR,
              from=toList[.x],
              to=toList[.x + 1],
              plot.points=FALSE,
              xlab="Active days per month",
              key=list(text=levels(top_10_Y1$QR),
                             lines=TRUE),
              main=paste("Percentile", names(toList)[.x], "to", names(toList[.x+1])))
})

do.call(grid.arrange,plots)



# adding summary stats to the graph that calculates and plots quantiles
q1 = ddply(top_10_Y1, .(QR), function(x){
  x1 = quantile(x$active_days, .25)
  y1 = density(x$active_days, from=x1, to=x1, n=1)$y # this calculates the density at a single point
  df.dens.x1 = data.frame(x=x1,y=y1)
  return(df.dens.x1)
}) # this returns the 25 percentile for each quarter

q2 = ddply(top_10_Y1, .(QR), function(x){

  x1 = quantile(x$active_days, .5)
  y1 = density(x$active_days, from=x1, to=x1, n=1)$y # this calculates the density at a single point
  df.dens.x1 = data.frame(x=x1,y=y1)
  return(df.dens.x1)
})

q3 = ddply(top_10_Y1, .(QR), function(x){
  x1 = quantile(x$active_days, .75)
  y1 = density(x$active_days, from=x1, to=x1, n=1)$y # this calculates the density at a single point
  df.dens.x1 = data.frame(x=x1,y=y1)
  return(df.dens.x1)
})

q = rbind(q1,q2,q3) # quantile data for geom plot
p2 = p1 + 
  geom_point(data=q, aes(x=x,y=y,group=factor(QR),shape=factor(QR),color=factor(QR),size=6))  

ggsave("top_10_domains.pdf",p2,height=8,width=16)


# data check
  # is every domain contributing to every quarter?
top_10_Y1 %>%
  group_by(domain_numeric) %>%
  n_distinct(QR)

# graph 3: grouped by 3/6/12/more months##

# for each user, add a sequence of active months
data2 = data %>%
  group_by(user_pk) %>%
  mutate(nmonths = seq(n())) 

# for each group, compute the distribution of active day percent
data_adp2 = tbl_df(as.data.frame(with(data2, table(cut(nmonths, breaks=c(1,3,6,12,50)), adp))))
data_adp2 = arrange(data_adp2, Var1) # Var1 is the resulting category
data_adp2 = data_adp2 %>%
  group_by(Var1) %>%
  mutate(dist_adp = Freq/n())

p_adp3 = ggplot(data_adp2,aes(x=adp,y=dist_adp,group=Var1))+
  geom_line(aes(linetype=Var1,color=Var1))+
  xlab("percent of active days")+
  ylab("distribution")+
  theme(
    panel.background = element_blank()
  )
ggsave("p_adp3.pdf",p_adp3,width=16,height=8)

# Graph 4: same with graph 3, only on users who are active for at least 15 of their first 18 months
user_nmonths = tally(group_by(data2, user_pk)) # this returns total active months
user_candidate = select(filter(user_nmonths, n >= 18), user_pk) # only users with at least 18 consecutive active months will be kept
data_adp3 = filter(data2, user_pk %in% user_candidate$user_pk)

adp3_sub_test = data_adp3 %>%
  group_by(user_pk) %>%
  summarise(seq_15 = length(row.names(table(seq(15)%in%month_index)))) # if a user is active for 15 consecutive months, row.names would have a length of 1, otherwise 2(TRUE,FALSE)
adp3_users = select(filter(adp3_sub_test, seq_15 == 1),user_pk) # this returns all qualified users
data_adp3 = filter(data_adp3, user_pk %in% adp3_users$user_pk) 

data_adp3 = tbl_df(as.data.frame(with(data_adp3, table(cut(nmonths, breaks=c(1,3,6,12,50)), adp))))
data_adp3 = arrange(data_adp3, Var1) # Var1 is the resulting category
data_adp3 = data_adp3 %>%
  group_by(Var1) %>%
  mutate(dist_adp = round(100*(Freq/sum(Freq))))
data_adp4 = filter(data_adp3, dist_adp > 0)

p_adp4 = ggplot(data_adp4,aes(x=adp,y=dist_adp,group=Var1))+
  geom_line(aes(linetype=Var1,color=Var1))+
  xlab("percent of active days")+
  ylab("distribution")+
  theme(
    panel.background = element_blank()
  )+
  ggtitle("422 users who are active for at least 15 of their first 18 months are included")

ggsave("p_adp4.pdf",p_adp4,width=16,height=8)

