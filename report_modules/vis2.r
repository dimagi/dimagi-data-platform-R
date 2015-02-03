#!/usr/bin/Rscript

####################################################################################
# code developed by Klurig Analytics (Dag Holmboe)
# Nov 05, 2014
# Please don't remove this section
####################################################################################

####################################################################################
rm(list=ls())
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))

################################################################
add.months <- function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]

################################################################
# main
# main
if( length( commandArgs() ) == 6 ) {
  input_file  <- commandArgs()[6]
  #output_file <- commandArgs()[7]
} else {
  cat("\nError: not correct parameters\n")
  cat("Usage: program <input file>\n\n")
  quit()
}

#input_file <- "../data/sample_monthly_original_vis2.csv"
#output_file  <- "output.csv"

p1 <- read.csv(input_file, stringsAsFactors=T)
p1 <- tbl_df(p1)

# only keep columns that are interesting
p1 <- p1 %>% select(domain_numeric, user_pk, calendar_month, previous_month_active, next_month_active) 
  
p1$calendar_month   <- as.Date(as.character(p1$calendar_month), format="%Y-%m-%d")

# used for filtering during development
#p1 <- p1 %>% filter(p1$user_pk == 5031)
#p1 <- p1 %>% filter(domain_numeric == 132 | domain_numeric == 29)
#p1 <- p1 %>% filter(domain_numeric == 179 | domain_numeric == 29)
#p1 <- p1 %>% filter(calendar_month >= "2014-05-01" & calendar_month <= "2014-09-30")  
#p1 <- p1 %>% filter(calendar_month >= "2014-07-01" & calendar_month <= "2014-07-30")  

# remove NAs for previous months and next months, set it all to FALSE
p1$previous_month_active[is.na(p1$previous_month_active)] <- FALSE
p1$next_month_active[is.na(p1$next_month_active)] <- FALSE

# create a new field for this month active (similar to previous and next month active)
p1$this_month_active <- TRUE

# create a data structure that includes all months from start to finish.  This data structure
#   will be used to join other data structures such that we have all months
min_month <- add.months(min(p1$calendar_month), -1)
max_month <- add.months(max(p1$calendar_month), 1)
all.months <- data.frame(calendar_month=seq(min_month,max_month, by="1 month"))

# join the two data structures such that we get one data structure that contains
#   all months with all entries.  Set all NAs to FALSE
df.all <- left_join(all.months, p1)
df.all$this_month_active[is.na(df.all$this_month_active)] <- FALSE
df.all$previous_month_active[is.na(df.all$previous_month_active)] <- FALSE
df.all$next_month_active[is.na(df.all$next_month_active)] <- FALSE

# count gains and losses
df.all$gain <- ifelse(!df.all$previous_month_active & df.all$this_month_active, 1, 0)
df.all$loss <- ifelse(df.all$this_month_active & !df.all$next_month_active, 1, 0)

# in this section, we copy months with a loss, eg next month is not active to one month ahead.
#   with other words, the "next month active" states that next month is active, but we don't know
#   which month that actually is.  For instance in June 2014, for a particular domain (say 7), we see
#   in the spreadsheet that next month is inactive. However, in the data structure, we might not
#   have July 2014. But we need to cound July 2014 as a loss (assuming we lost a person who 
#   was active in June but not in July). So therefore, we will copy the data structure to a
#   new data structure, but only for those months where next month is inactive.
#   After the data structure has been copied, then we shift the data structure one month 
#   forward (we add 1 month to each months, eg we shifted from June 2014 to July 2014.
#   The we bind this new data structure back to the original data structure, 
#   re-order the calendar month and the results is a new data structure where we 
#   simply can count the gains for the current month while at the same time also count
#   the losses for the current month.
df.tmp <- df.all[df.all$loss==1,]
df.all$loss[df.all$loss==1] <- 0
df.tmp$calendar_month <- as.Date(sapply(df.tmp$calendar_month, function(x) add.months(x,1)), origin="1970-01-01")
df.tmp$this_month_active     <- FALSE
df.tmp$previous_month_active <- TRUE 
df.tmp$next_month_active     <- FALSE
df.tmp$gain <- 0
df.all <- rbind(df.all, df.tmp)
df.all <- df.all[order(df.all$calendar_month),]
df.all <- df.all %>% filter(!is.na(domain_numeric))
df.all <- df.all %>% group_by(calendar_month, domain_numeric)

# calculate total gains and losses
df2 <- df.all %>% summarize(
  gain_s = sum(gain),
  loss_s = sum(loss))
df2$domain_numeric <- as.factor(df2$domain_numeric)

# calculate active, which is done like this:
# active(this month) = active(last month) + gains - losses

df2 <- df2 %>% group_by(domain_numeric)
df2.tmp2 <- data.frame()
for( dn in unique(df2$domain_numeric) )
{
  df2.tmp1 <- filter(df2, domain_numeric == dn )
  df2.tmp1$active <- 0 
  df2.tmp1$active[1] <- df2.tmp1$gain_s[1] - df2.tmp1$loss_s[1]
  for( i in 2:nrow(df2.tmp1) )
  {
      df2.tmp1$active[i] <- df2.tmp1$active[i-1] + df2.tmp1$gain_s[i] - df2.tmp1$loss_s[i]
  }
  df2.tmp2 <- rbind(df2.tmp2, df2.tmp1)
}

# calculate delta:
# delta = gains minus losses
df2.tmp2$delta <- df2.tmp2$gain_s - df2.tmp2$loss_s 

# change order
df2.tmp2 <- df2.tmp2[,c(1,2,3,4,6,5)]

##################
# before graphing, filter to last year, and set up title for charts
start_month <- "2013-10-01" 
end_month <- "2014-09-30"
title <- paste0("(", start_month, " - ", end_month, ")")

# start graphing
p2 <- df2.tmp2
p3 = group_by(p2, calendar_month, domain_numeric)
p3 <- p3 %>% filter(calendar_month >= start_month & calendar_month <= end_month )  

#### A. do googleVis interactive graphs
library(googleVis)
# create a line graph with x=date and y=number of procedures per month, line = CPTs

if(0) {
webDir <- "/var/www/dimagi"

# table
p3 <- group_by(p3, calendar_month, domain_numeric)
t <- gvisTable(p3, options=list(width=600, height=700))
#plot(t)
filex <- "table1.html"
cat(t$html$chart, file=file.path(webDir, filex))

# motion chart
# motion chart can not handle dplyr data frames
m <- gvisMotionChart(as.data.frame(p3), idvar="domain_numeric", timevar="calendar_month")
#plot(m)
filex <- "motion1.html"
cat(m$html$chart, file=file.path(webDir, filex))

# annotade time line chart
n2 <- gvisAnnotatedTimeLine(as.data.frame(p3),  datevar="calendar_month",
  numvar="active",  idvar="domain_numeric")
  options=list( width=600, height=700, fill=80)
#plot(n2)
filex <- "annotation2.html"
cat(n2$html$chart, file=file.path(webDir, filex))
}

# other plots 

# B. line graph - this is not great
p3 <- group_by(p2, calendar_month, domain_numeric)
p3 <- p3 %>% filter(calendar_month >= start_month & calendar_month <= end_month )  
ggplot(p3, aes(x=calendar_month, y=gain_s, color=domain_numeric, group=domain_numeric)) + geom_line() + geom_point() +
  theme_bw() + xlab("Calendar Month") + ylab("Monthly Gains") +  ggtitle(paste0("Gains per Month\n", title))
ggplot(p3, aes(x=calendar_month, y=loss_s, color=domain_numeric, group=domain_numeric)) + geom_line() + geom_point() +
  theme_bw() + xlab("Calendar Month") + ylab("Monthly Losses") +  ggtitle(paste0("Losses per Month\n", title))
ggplot(p3, aes(x=calendar_month, y=delta, color=domain_numeric, group=domain_numeric)) + geom_line() + geom_point() +
  theme_bw() + xlab("Calendar Month") + ylab("Monthly Delta") +  ggtitle(paste0("Delta per Month\n", title))
ggplot(p3, aes(x=calendar_month, y=active, color=domain_numeric, group=domain_numeric)) + geom_line() + geom_point() +
  theme_bw() + xlab("Calendar Month") + ylab("Monthly Active ") +  ggtitle(paste0("Active per Month\n", title))

# C. box plot
p3 <- group_by(p2, calendar_month, domain_numeric)
p3 <- p3 %>% filter(calendar_month >= start_month & calendar_month <= end_month )  
ggplot(p3, aes(domain_numeric, gain_s)) + geom_boxplot(outlier.colour='red', outlier.size=3) +
  theme_bw() + xlab("Domain") + ylab("Monthly Gains") +  ggtitle(paste0("Gains per Month\n", title))
ggplot(p3, aes(domain_numeric, loss_s)) + geom_boxplot(outlier.colour='red', outlier.size=3) +
  theme_bw() + xlab("Domain") + ylab("Monthly Losses") +  ggtitle(paste0("Losses per Month\n", title))
ggplot(p3, aes(domain_numeric, delta)) + geom_boxplot(outlier.colour='red', outlier.size=3) +
  theme_bw() + xlab("Domain") + ylab("Monthly Delta") +  ggtitle(paste0("Delta per Month\n", title))
ggplot(p3, aes(domain_numeric, active)) + geom_boxplot(outlier.colour='red', outlier.size=3) +
  theme_bw() + xlab("Domain") + ylab("Monthly Active ") +  ggtitle(paste0("Active per Month\n", title))

# D. bar chart
p3 <- group_by(p2, calendar_month, domain_numeric)
p3 <- p3 %>% filter(calendar_month >= start_month & calendar_month <= end_month )  
ggplot(p3, aes(x=calendar_month, y=gain_s, fill=domain_numeric)) + geom_bar(stat='identity', position="dodge") +
  theme_bw() + xlab("Month") + ylab("Gains") +  ggtitle(paste0("Gains per Month\n", title))
ggplot(p3, aes(x=calendar_month, y=loss_s, fill=domain_numeric)) + geom_bar(stat='identity', position="dodge") +
  theme_bw() + xlab("Month") + ylab("Losses") +  ggtitle(paste0("Losses per Month\n", title))
ggplot(p3, aes(x=calendar_month, y=delta, fill=domain_numeric)) + geom_bar(stat='identity', position="dodge") +
  theme_bw() + xlab("Month") + ylab("Delta") +  ggtitle(paste0("Delta per Month\n", title))
ggplot(p3, aes(x=calendar_month, y=active, fill=domain_numeric)) + geom_bar(stat='identity', position="dodge") +
  theme_bw() + xlab("Month") + ylab("Active") +  ggtitle(paste0("Active per Month\n", title))
ggplot(p3, aes(x=calendar_month, y=gain_s, fill=domain_numeric)) + geom_bar(stat='identity') +
  theme_bw() + xlab("Month") + ylab("Gains") +  ggtitle(paste0("Gains per Month\n", title))
ggplot(p3, aes(x=calendar_month, y=loss_s, fill=domain_numeric)) + geom_bar(stat='identity') +
  theme_bw() + xlab("Month") + ylab("Losses") +  ggtitle(paste0("Losses per Month\n", title))
ggplot(p3, aes(x=calendar_month, y=delta, fill=domain_numeric)) + geom_bar(stat='identity') +
  theme_bw() + xlab("Month") + ylab("Delta") +  ggtitle(paste0("Delta per Month\n", title))
ggplot(p3, aes(x=calendar_month, y=active, fill=domain_numeric)) + geom_bar(stat='identity') +
  theme_bw() + xlab("Month") + ylab("Active") +  ggtitle(paste0("Active per Month\n", title))

# E. facet on the domain
ggplot(p3, aes(x=calendar_month, y=gain_s, fill=domain_numeric)) + geom_bar(stat='identity') + 
  theme_bw() + xlab("Month") + ylab("Gains") +  ggtitle(paste0("Gains per Month\n", title)) + 
  theme(legend.position="none") + facet_wrap(~domain_numeric)
ggplot(p3, aes(x=calendar_month, y=loss_s, fill=domain_numeric)) + geom_bar(stat='identity') + 
  theme_bw() + xlab("Month") + ylab("Losses") +  ggtitle(paste0("Losses per Month\n", title)) + 
  theme(legend.position="none") + facet_wrap(~domain_numeric)
ggplot(p3, aes(x=calendar_month, y=delta, fill=domain_numeric)) + geom_bar(stat='identity') + 
  theme_bw() + xlab("Month") + ylab("Delta") +  ggtitle(paste0("Delta per Month\n", title)) + 
  theme(legend.position="none") + facet_wrap(~domain_numeric)
ggplot(p3, aes(x=calendar_month, y=active, fill=domain_numeric)) + geom_bar(stat='identity') + 
  theme_bw() + xlab("Month") + ylab("Active") +  ggtitle(paste0("Active per Month\n", title)) + 
  theme(legend.position="none") + facet_wrap(~domain_numeric)


