# time series visualization of all data points included in round 2 analysis

# multivariate time series plot
# create a matrix of users and calendar months
dm = select(working_data_1, user_pk, domain_numeric)
d = select(working_data_1, user_pk, month.index, active_days)
dlist = split(d, d$user_pk)
m = c(min(d$month.index), max(d$month.index))
d2 = data.frame(seq(m[1],m[2],by="month"))
names(d2) = c("month.index")
d2 = tbl_df(d2)
d_user = expand.grid(user_pk=unique(d$user_pk),month.index=d2$month.index)
d_full = tbl_df(left_join(d_user,d,by=c("month.index","user_pk"),all=TRUE)) # already ordered 
d_full = arrange(d_full, month.index, user_pk)

# d_full should be used to construct the matrix for mtvs plot
dmat = matrix(d_full$active_days, 
              nrow=nrow(d2),ncol=n_distinct(d$user_pk),
              byrow=TRUE,
              dimnames=list(as.character(unique(d_full$month.index)),
                            unique(d_full$user_pk)))
png(filename="mvts_div2.png", 
    width=720, 
    height=1080, 
    res=144,
    bg="transparent")
dmat_ts = mvtsplot(dmat, norm="global")
dev.off()

# Motion Chart
# each bubble is a unique user (or domain?)
# bubble size: number of unique users in this domain
# x-axis: calendar month
# y-axis: median active days

suppressPackageStartupMessages(library(googleVis))
library(dplyr)
tsData = select(working_data_1, 
                user_pk, 
                domain_numeric,
                ncases_touched,
                active_days,
                month.index)

tsData = na.omit(tsData)

tsData_1 = tsData %>%
  group_by(domain_numeric, month.index) %>%
  summarise_each(funs(sum)) %>%
  select(., domain_numeric, month.index, ncases_touched, active_days)

tsData_2 = tsData %>%
  group_by(domain_numeric, month.index) %>%
  summarise(nusers_active = n_distinct(user_pk))

inner_join(tsData_1, tsData_2, by=c("domain_numeric", "month.index"))

plot(gvisMotionChart(tsData_1, idvar='domain_numeric', timevar='month.index'))

