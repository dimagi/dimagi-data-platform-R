####################
##HQ Visualization##
####################

library(ggplot2)
library(scales)
library(plyr)
library(dplyr)
library(grDevices)
library(grid)

data <- tbl_df(read.csv("data.csv", stringsAsFactors = FALSE))
data$calendar_month <- as.Date(data$calendar_month,format="%m/%d/%Y")
## manually delete a user: user_pk == 6421
data = filter(data, user_pk != 6421)

# for each domain, count total unique users 
tuu <- data %>%
  group_by(domain_numeric) %>%
  summarise(tot_unique_users = n_distinct(user_pk))
tuu = arrange(tuu, tot_unique_users)
# rank domains by total number of users
tuu$dm_rank_tuu = seq(nrow(tuu))

# for each domain each month, count blocks of users
  # blocks: (0,25],(25,50]... 
  # expected result: in a domain this month, there would be N blocks of users
bou = data %>%
  group_by(domain_numeric, calendar_month) %>%
  summarise(user_size = n_distinct(user_pk))

bou = tbl_df(cbind(bou,findInterval(bou$user_size, c(25,50,75,100,125,150,175,200,225,250,275,300,325))))
colnames(bou)[4] = c("user_block")
bou$user_block = bou$user_block + 1
rep_domain = rle(bou$domain_numeric)$lengths
bou$dm_index = rep(seq(length(unique(bou$domain_numeric))),rep_domain) # adding a new index to correct for the random domain_numeric (hitting 7600s)

bou = arrange(bou, dm_index, calendar_month)
p_bou = ggplot(bou, aes(x=calendar_month,y=dm_index,fill=factor(dm_index)))+
  geom_tile(aes(height=user_block))+
  scale_fill_manual(values=c(rep(c("blue","yellow"),times=153)))
# customize y-axis scale to be same with the height of geom tiles


# for each domain, count total unique months
tum <- data %>%
  group_by(domain_numeric) %>%
  summarise(tot_unique_months = n_distinct(calendar_month))
tum <- arrange(tum, tot_unique_months)
tum$dm_rank_tum <- seq(nrow(tum))

# cbind by domain_numeric
tuu <- arrange(tuu, domain_numeric)
tum <- arrange(tum, domain_numeric)
cbinded <- tbl_df(merge(tuu, tum))

# add dm_rank_tuu, dm_rank_tum, user_bin back to monthly aggregate data
rep_time <- as.data.frame(table(data$domain_numeric))$Freq
cbinded_rep <- cbinded[rep(row.names(cbinded),rep_time),1:6]
data <- arrange(data, domain_numeric)
hq_viz_data <- tbl_df(cbind(data, cbinded_rep[,2:6]))

# rank domains by total unique users (ascending)
hvd <- arrange(hq_viz_data, dm_rank_tuu, user_pk, calendar_month)
rep_time_user <- rle(hvd$user_pk)$lengths
hvd$user_rank_1 <- rep(seq(length(unique(hvd$user_pk))), rep_time_user)


# SETUP PARAMS
# title text style 
title.style <- element_text(family="Arial", face="bold", color="black", size=16)
# axis text style
label.style <- element_text(family="Arial Narrow", color="grey50", size=8, angle=90)
# panel setup
horizontal_theme <- theme(  
  panel.background    = element_blank() ,
  panel.grid.major.x  = element_blank() ,
  panel.grid.minor    = element_blank() ,
  axis.text.y = element_blank(),
  axis.title.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.line  = element_line( size=.2 , colour="#666666" ),
  legend.position = "none", # turn off legends
  plot.margin = unit( c(.2,.25,.25,.5) , "in")
) 
non_horizontal_theme <- theme(
  panel.background    = element_blank() ,
  #  panel.grid.major.x  = element_blank() ,
  panel.grid.minor.y    = element_blank() ,
  axis.line  = element_line( size=.2 , colour="#666666" ),
  legend.position = "none", # turn off legends
  plot.margin = unit( c(.5,.25,.25,.5) , "in") 
)

# horizontal chart 1: each line represents one user. lines are grouped by domains
monthStart <- min(hvd$calendar_month)
monthEnd <- max(hvd$calendar_month)
p_hvd = ggplot(hvd, aes(x=user_rank_1,y=calendar_month)) + 
  geom_tile(aes(fill=factor(dm_rank_tuu))) + 
  scale_fill_manual(values=rep(c("darkgoldenrod1","azure3"),times=26354)) +
  scale_y_date(limits=c(monthStart,monthEnd),breaks=date_breaks(width="1 month"),labels=date_format("%m/%y")) +
  theme(  
    panel.background    = element_blank() ,
    panel.grid.major.x  = element_blank() ,
    panel.grid.minor.y  = element_blank() ,
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line  = element_line( size=.2 , colour="#666666" ),
    legend.position = "none", # turn off legends
    plot.margin = unit( c(.2,.25,.25,.5) , "in"),
    aspect.ratio = 1
  ) +
  ggtitle(expression(atop(italic("Domain activity over time"), 
                          atop("Each line represents one user",
                               atop("Two colors are recycled to distinguish between 264 domains", ""))))) +
  ylab("calendar month") +
  coord_flip() 

# horizontal chart 1.2: one line per domain, domain(geom tile) height is proportional to its user size
hvd2 = filter(hvd, domain_numeric < 310)
hvd2 = arrange(hvd2, user_bin, dm_rank_tuu)
p_hvd_1.1 = ggplot(hvd2, aes(x=dm_rank_tuu,y=calendar_month,fill=factor(user_bin)))+
  geom_tile(aes(height=user_bin*5))+
  scale_y_date(limits=c(monthStart,monthEnd),breaks=date_breaks(width="1 month"),labels=date_format("%m/%y")) +
  theme(  
    panel.background    = element_blank() ,
    panel.grid.major.y  = element_blank() ,
    panel.grid.minor.x  = element_blank() ,
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 10, angle = 0),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line  = element_line( size=.2 , colour="#666666" ),
    legend.position = "none", # turn off legends
    plot.margin = unit( c(.2,.25,.25,.5) , "in"),
    aspect.ratio = 1
  )

ggsave("hq_1.1.pdf", p_hvd_1.1, width=12, height=12)
# horizontal chart 2: for each domain, plot a horizontal line visualizing their total active days
# for each domain each month, calculate total active days
hvd_tad = hvd %>%
  group_by(dm_rank_tuu, calendar_month) %>%
  summarise(tot_active_days = sum(active_days))

p_hvd_tad = ggplot(hvd_tad, aes(x=dm_rank_tuu,y=calendar_month)) +
  geom_tile(aes(fill=factor(dm_rank_tuu),alpha=log(tot_active_days))) + 
  scale_fill_manual(values=rep(c("darkgoldenrod1","azure3"),times=132)) +  
  scale_alpha(range=c(0.2,1))+
  theme(  
    panel.background    = element_blank() ,
    panel.grid.major.x  = element_blank() ,
    panel.grid.minor.y  = element_blank() ,
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line  = element_line( size=.2 , colour="#666666" ),
    legend.position = "none", # turn off legends
    plot.margin = unit( c(.2,.25,.25,.5) , "in"),
    aspect.ratio = 1
  ) +
  scale_y_date(breaks=date_breaks("months"), labels=date_format("%m/%y")) +    
  ggtitle(expression(atop(italic("Domain activity: total active days per month"), 
                          atop("Each line = one domain", 
                               atop("Darker colors = more active days", ""))))) +
  ylab("calendar month") +
  coord_flip() 

# horizontal chart 3: for each domain, plot difference in total active days between this and previous month
arrange(hvd_tad, dm_rank_tuu, calendar_month)
hvd_tad = mutate(hvd_tad, prev = lag(tot_active_days))
hvd_tad = mutate(hvd_tad, tad_diff=ifelse(is.na(prev), "NA", tot_active_days-prev))
hvd_tad = mutate(hvd_tad, tad_diff_cat=ifelse(tad_diff < 0, 0, 1)) # assign val=1 if one domain is more active this month than last month in terms of days
hvd_tad$tad_diff_cat[which(hvd_tad$tad_diff == "NA")] = NA

# deprecated 
p_hvd_tdc = ggplot(hvd_tad, aes(x=dm_rank_tuu,y=calendar_month)) +
  geom_tile(aes(fill=factor(dm_rank_tuu),alpha=factor(tad_diff_cat))) + 
  scale_fill_manual(values=rep(c("darkgoldenrod1","azure3"),times=132)) +  
  scale_alpha_discrete(range=c(0.2,1))+
  theme(  
    panel.background    = element_blank() ,
    panel.grid.major.x  = element_blank() ,
    panel.grid.minor.y  = element_blank() ,
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line  = element_line( size=.2 , colour="#666666" ),
    legend.position = "none", # turn off legends
    plot.margin = unit( c(.2,.25,.25,.5) , "in"),
    aspect.ratio = 1
  ) +
  scale_y_date(breaks=date_breaks("months"), labels=date_format("%m/%y")) + 
  ggtitle(expression(atop(italic("Domain activity: changes in total active days over time"), 
                          atop("Each line = one domain (264 domains in total)",
                               atop("Current vs. Previous Month: Darker color represents a growth in active days", ""))))) +
  ylab("calendar month") +  
  coord_flip() 

# counting total positive values of tad_diff_cat and order domains by count
dm_rank_tp = hvd_tad %>%
  group_by(dm_rank_tuu) %>%
  summarise(tot_pos = sum(tad_diff_cat[which(tad_diff_cat == 1)]))

dm_rank_tp = arrange(dm_rank_tp, tot_pos)
dm_rank_tp$dm_rank_pos_prev = seq(nrow(dm_rank_tp))

# add the new ranking back to monthly data
rep_time_2 = as.data.frame(table(hvd_tad$dm_rank_tuu))$Freq
dm_rank_tp = arrange(dm_rank_tp, dm_rank_tuu)
dm_rank_tp_rep = dm_rank_tp[rep(row.names(dm_rank_tp),rep_time_2),1:3]
hvd_tad = arrange(hvd_tad, dm_rank_tuu)
hvd_tad <- tbl_df(cbind(hvd_tad, dm_rank_tp_rep[,2:3]))

hvd_tad = arrange(hvd_tad, dm_rank_pos_prev, calendar_month)

# colors()[grep("azure",colors())] # returning all color names containing 'azure'

p_hvd_tad_2 = ggplot(hvd_tad, aes(x=dm_rank_pos_prev,y=calendar_month)) +
  geom_tile(aes(fill=factor(dm_rank_pos_prev),alpha=factor(tad_diff_cat))) + 
  scale_fill_manual(values=rep(c("darkgoldenrod1","azure3"),times=132)) +  
  scale_alpha_discrete(range=c(0.3,1))+
  theme(  
    panel.background    = element_blank() ,
    panel.grid.major.x  = element_blank() ,
    panel.grid.minor.y  = element_blank() ,
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line  = element_line( size=.2 , colour="#666666" ),
    legend.position = "none", # turn off legends
    plot.margin = unit( c(.2,.25,.25,.5) , "in"),
    aspect.ratio = 1
  ) +
  scale_y_date(breaks=date_breaks("months"), labels=date_format("%m/%y")) + 
  ylab("calendar month") +
  ggtitle(expression(atop(italic("Domain activity: changes in total active days over time"), 
                          atop("Current vs. Previous Month: Darker color represents an increase in active days", "")))) +
  coord_flip() 

ggsave("1.4_horizontal_domain_active_or_not.pdf", p_hvd, path=mypath, width=12, height=12, limitsize = FALSE, useDingbats=FALSE)
ggsave("1.3_horizontal_domain_total_active_days_by_domain.pdf", p_hvd_tad, path=mypath, width=12, height=12, limitsize = FALSE)
ggsave("1.3_horizontal_domain_change_in_total_active_days_by_domain.pdf", p_hvd_tad_2, path=mypath, width=12, height=12, limitsize = FALSE)


#################
# NON-HORIZONTAL#
#################
# for each domain each month, counts total unique users 
tau <- data %>%
  group_by(domain_numeric, calendar_month) %>%
  summarise_each(funs(n_distinct), user_pk)

p_tau <- ggplot(tau, aes(x=calendar_month, y=user_pk,group=factor(domain_numeric))) + 
  geom_line(aes(colour=factor(domain_numeric),size=0.5)) +
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%m/%y")) + 
  scale_y_discrete("user_pk", breaks = c(0,50,100,200,300,400,500)) +
  labs(x="calendar month", y="total users") + 
  ggtitle(expression(atop("Total active users on each domain",
                          atop(italic("Aesthetic mapping: color - domain"),"")))) + 
  non_horizontal_theme 

# for each domain in each month, calculate the change in number of users from the previous month (user change)
uc <- data %>% 
  group_by(domain_numeric, calendar_month) %>%
  summarise(tot_active_users = n_distinct(user_pk)) %>%
  mutate(change = tot_active_users - lag(tot_active_users)) # to be rewritten with tally() function

# bar chart
p_uc_bar <- ggplot(uc, aes(x=calendar_month)) + 
  geom_bar(aes(y=change, fill=factor(domain_numeric)),stat="identity") + 
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%m/%y")) + 
  labs(x="calendar month", y="change compared to previous month") +
  ggtitle(expression(atop("Changes in active users on each domain",
                          atop(italic("Aesthetic mapping: color - domain"),"")))) +
  non_horizontal_theme +
  theme(axis.text.x=element_text(angle=90))+
  theme(title=title.style)

# for each domain, each month of a year, count total active days (of each user) and sort in descending order 
tad <- data %>% 
  group_by(domain_numeric, calendar_month) %>%
  summarise(tot_active_days = sum(active_days)) 

# TO BE UPDATED: Y-AXIS & LEGENDS 
p_tad <- ggplot(tad, aes(x=calendar_month, y=tot_active_days,group=factor(domain_numeric))) + 
  geom_line(aes(colour=factor(domain_numeric), size=1)) +
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%m/%y")) + 
  scale_y_discrete("tot_active_days", breaks = c(500,1000,1500,2000,2500,3000,3500,4000,4500)) +
  labs(list(x="calendar month", y="total active days")) +
  ggtitle(expression(atop("Total active days on each domain",
                          atop(italic("Aesthetic mapping: color - domain"),"")))) +
  non_horizontal_theme +
  theme(title=title.style) +
  theme(axis.text.x=element_text(angle=90))


# limit data submitted to submissions from 2012-10-31 to 2014-10-31 for all visuals 
tau_sub <- filter(tau, calendar_month >= as.Date("2012-10-31") & calendar_month <= as.Date("2014-10-31"))
tad_sub <- filter(tad, calendar_month >= as.Date("2012-10-31") & calendar_month <= as.Date("2014-10-31"))

# for 10 skyrocketing months in y-axis variables, identity their domain_numeric
tau_skyrockets <- arrange(tau_sub, desc(user_pk))[1:10,]
tad_skyrockets <- arrange(tad_sub, desc(tot_active_days))[1:10,]

# bar chart viz: total active users
tau_sub <- arrange(tau_sub, calendar_month, user_pk, domain_numeric)
p_tau_sub <- ggplot(tau_sub, aes(x=calendar_month)) + 
  geom_bar(aes(y=(user_pk), fill=factor(domain_numeric)), # need to add color scale to distinguish adjacent domains with same color fill 
           stat="identity", 
           width=20) +
  scale_fill_manual(values=rep(brewer.pal(10,"Set3"),times=26)) + 
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%m/%y")) + 
  non_horizontal_theme + 
  labs(x="calendar month", 
       y="total active users") +
  ggtitle(expression(atop("Total active users on each domain in last two years",
                          atop(italic("Aesthetic mapping: color - domain, size - total users",""))))) +
  theme(title=title.style)


# bar chart viz: total active days
tad_sub <- arrange(tad_sub, calendar_month, tot_active_days, domain_numeric)
p_tad_sub <- ggplot(tad_sub, aes(x=calendar_month)) + 
  geom_bar(aes(y=(tot_active_days), fill=factor(domain_numeric)), # need to add color scale to distinguish adjacent domains with same color fill 
           stat="identity", 
           width=20) +
  scale_fill_manual(values=rep(brewer.pal(10,"Set3"),times=26)) + 
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%m/%y")) + 
  horizontal_theme + 
  labs(x="calendar month", 
       y="total active days") +
  ggtitle(expression(atop("Total active days on each domain in last two years",
                          atop(italic("Aesthetic mapping: color - domain, size - active days",""))))) +
  theme(title=title.style)


# log scale y-axis variables
bu_sub <- data[,c("domain_numeric","business_unit")]
# find unique domain_numeric
bu_sub <- bu_sub[!duplicated(bu_sub$domain_numeric),]
tad_sub_2 <- inner_join(tad_sub, bu_sub, by = "domain_numeric")
tad_sub_2$log_tot_active_days <- round(log2(tad_sub_2$tot_active_days),1)
tad_sub_2 <- arrange(tad_sub_2, domain_numeric, calendar_month)
tad_sub_2$bu <- ifelse(tad_sub_2$business_unit == "DSI", "1", "0")
tad_sub_2[which(is.na(tad_sub_2$bu)),]$bu = 0

p_tad_sub_2 <- ggplot(tad_sub_2, aes(x=calendar_month, y=log_tot_active_days, group=factor(domain_numeric))) + 
  geom_line(aes(color=factor(bu),size=factor(bu))) + # need to add color scale to distinguish adjacent domains with same color fill 
  scale_size_manual(values=c(0.05, 1.2)) + 
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%m/%y")) + 
  non_horizontal_theme +
  labs(x="calendar month", 
       y="total active days"
  ) +
  ggtitle(expression(atop("Total active days on each domain in last two years (log scale)",
                          atop(italic("Thick blue: DSI domains, Thin Pink: Non-DSI domains"),"")))) +
  theme(title=title.style)


# path to save plot to (each version should be stored in different sub-folders)
mypath=file.path("D://Dropbox//Dropbox (Dimagi)//data tests", "motion_chart", "1.1")

ggsave("1.2_domain_total_active_days_log_scale.pdf", p_tad_sub_2, path=mypath, width=12, height=8)
ggsave("1.1_domain_total_active_days_bar_chart.pdf", p_tad_sub, path=mypath, scale=2)
ggsave("1.2_domain_total_active_users.pdf", p_tau, path=mypath, width=12, height=8)
ggsave("1.2_domain_active_user_change.pdf", p_uc_bar, path=mypath, width=12, height=8)
ggsave("1.2_domain_total_active_days.pdf", p_tad, path=mypath, width=12, height=8)

# polar histogram

p_hvd_tad_3 = ggplot(hvd_tad, aes(x=dm_rank_pos_prev,y=calendar_month)) +
  geom_tile(aes(fill=factor(dm_rank_pos_prev),alpha=factor(tad_diff_cat))) + 
  scale_fill_manual(values=rep(c("darkgoldenrod1","azure3"),times=132)) +  
  scale_alpha_discrete(range=c(0.3,1))+
  horizontal_theme + 
  theme(title=title.style,
        aspect.ratio=1) +  
  scale_y_date(breaks=date_breaks("months"), labels=date_format("%m/%y")) + 
  ggtitle(expression(atop(italic("Domain activity: changes in total active days over time"), 
                          atop("Current vs. Previous Month: Darker color represents an increase in active days", "")))) +
  coord_flip() +
  coord_polar()

ggsave("1.2_domain_change_active_days_polar.pdf", p_hvd_tad_3, path=mypath, scale=2)