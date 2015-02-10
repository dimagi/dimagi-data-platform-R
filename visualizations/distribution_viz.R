##############################
##distribution visualization##
##############################

# distribution visualization #

# create a single line graph from a set of user-months and an activity indicator showing distribution of levels   
  # For each indicator
  # the x-axis should be the value of the indicator
  # the y-axis should be the percent of user-months that have that value

# y-axis indicator 1: percent of active days
# multiple graphs will be generated on different sets of user-months 
# graph naming format: data_adp_n (n: the nth graph)

data <- tbl_df(read.csv("blog_data_2_2_15.csv", stringsAsFactors = FALSE))
data$adp <- round(data$active_day_percent) # rounding would make it a discrete variable

library(plyr)
library(dplyr)
library(ggplot2)
library(Cairo)
library(zoo)
library(RColorBrewer)

# construct data table for graph 1
adp_groups <- data %>% group_by(adp) # order data table by percent of active days
x <- unique(adp_groups$adp) # extract unique percent of active days 
y <- group_size(adp_groups) # calculate the occurrence of each value in percent of active days
data_adp1 <- tbl_df(as.data.frame(cbind(x,y))) # combine percentage of each value occurring and unique values into a new data table
data_adp1 <- arrange(data_adp1, x) # reorder the data table by unique values in percent of active days
data_adp1$y_prob <- round(y/sum(data_adp1$y),digits=6) # calculate the percentage of each value occurring in the whole data set

# Distribution Graphs 1: Kernel Density Plot for all user-months
png(600,600,file="pad_all_domains.png")
plot(data_adp1$x, data_adp1$y_prob, type="l",
     main="All Domains: Percent of active days per month",
     xlab="Percent of active days",
     ylab="Percent of user-months having the value"
)
box()
dev.off()


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
  mutate(month_seq = seq(n())) # for each user this calculates the total active month 

# for each user on each domain, categorize active month by quarter (not calendar quarter)
top_10$YR = ifelse(top_10$month_seq <= 12, "Y1","Y2")
top_10_Y1 = filter(top_10, YR == "Y1")
top_10_Y1$QR = sapply(top_10_Y1$month_seq, function(x){
  if(x <= 3) return("Q1")
  if(x > 3 & x <= 6) return("Q2")
  if(x > 6 & x <= 9) return("Q3")
  else return("Q4")
})

# for each quarter, generates the percentage distribution of active days percent. User may contribute to multiple lines
DIST = ddply(top_10_Y1, .(QR), function(x){
  y = as.data.frame(table(x$adp))
  return(y)
}) 
DIST = DIST %>%
  group_by(QR) %>%
  mutate(y_prob = Freq/sum(Freq))

breaks_x = order(unique(DIST$Var1))
p_by_QR = ggplot(DIST, aes(x=Var1,y=y_prob,group=factor(QR)))+
  geom_line(aes(color=QR))+
  scale_x_discrete(breaks=breaks_x)
  scale_color_manual(values=rep(brewer.pal(4,"Set3"),times=5))+
  facet_grid(.~year)+
  xlab("Percent of active days per month")+
  ylab("Percent of user-months having these value")+
  theme(
    panel.background = element_blank(),
    legend.position = "none",
    panel.grid.minor.x  = element_blank() 
  )

ggsave("distribution_by_quarter_big_10.png",p_by_QR,width=16,height=8)

# annotate lines with points at each quantile
quantiles = ddply(DIST, .(QR), function(x){
  y = quantile(x$Var1, probs=c(0.25, 0.50, 0.75)) # this returns the quantile points of adp
  return(y)
})
colnames(quantiles) = c("QR","1st","2nd","3rd")

quantiles = reshape(quantiles,
        varying=c("1st","2nd","3rd"),
        v.names="quantile",
        timevar="quantile",
        idvar="QR",
        direction="long") # reshape a wide data frame to a long one
quantiles = arrange(quantiles,QR)
quantiles$quantile = round(quantiles$quantile)
colnames(quantiles)[2] = c("Var1")
# join quantiles and DIST
annotation_points = inner_join(quantiles,DIST)

# add annotation plots to the quarterly plot
p_adp2 = ggplot(NULL, aes(Var1,y_prob,group=factor(QR),color=QR)) +
  geom_line(data=DIST)+
  scale_x_discrete(breaks=breaks_x) +
  geom_point(data=annotation_points,aes(size=1,shape=QR))
  
ggsave("top_10_domains_with_points.png",p_adp2,height=8,width=16)


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

