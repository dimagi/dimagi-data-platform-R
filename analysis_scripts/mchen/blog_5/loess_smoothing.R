# Element: Point(position(user_pk, percent_active_days))
# Dim(1): percent of active days
# Dim(2): ranksource("data_prep_no_gap.R")

# functions
  # user-month percent active days
loessTbl = function(var){
  var = round(var, digits = 2)
  m1 = as.data.frame(table(var))
  names(m1) = c("percent_active_days", "count")
  m1$pct = round(m1$count/sum(m1$count), digits = 4)
  m1$percent_active_days = as.numeric(as.character(m1$percent_active_days))
  return(m1)
}

binning = function(x){
  x$bins = cut(x$percent_active_days, breaks = 0.05*(0:20), 
               labels = c("0.05","0.10","0.15","0.20","0.25","0.30","0.35","0.40",
                          "0.45","0.50","0.55","0.60","0.65","0.70","0.75",
                          "0.80","0.85","0.90","0.95","1.0"), 
               include.lowest = TRUE)
  return(x)
}

dataPerUser = function(dataList){
  d1 = lapply(dataList, function(x){
    y = x %>%
      group_by(user_pk) %>%
      summarise(pad = mean(percent_active_days)) # obtain average %days for each unique user
    z=loessTbl(y$pad)
    return(z)    
  })
}

singleBinnedPlot = function(d1){
  d1 = binning(d1)
  d2 = d1 %>%
    group_by(bins) %>%
    summarise(percent = sum(pct))
  return(d2)
}

binnedDataPlot = function(d1) {
  d2 = lapply(d1, function(x)binning(x))
  d3 = lapply(d2, function(x){
    y = x %>%
      group_by(bins) %>%
      summarise(percent = sum(pct))
    return(y)
  })
  for (i in 1:length(d3)){
    d3[[i]]$facet = names(d3)[i]
  }
  d4 = do.call(rbind, d3)
  return(d4)
}


# Data: all data excluding first 3 months
# Figure 1.1 %days, user-month, excluding gap months 
data_1_1 = loessTbl(p1$percent_active_days)
data_1_1_binned = singleBinnedPlot(data_1_1)

# Figure 1.3 %days, per user, excluding gap months
data_1_3 = p1 %>%
  group_by(domain_numeric, user_pk) %>%
  summarise(pad = mean(percent_active_days)) 
data_1_3 = loessTbl(data_1_3$pad)
data_1_3 = singleBinnedPlot(data_1_3)

# Data: all data excluding first 6 months
# Figure 2.1 %days, user-month, excluding gap months
data_2_1 = loessTbl(p2$percent_active_days)
data_2_1_binned = singleBinnedPlot(data_2_1)

# Loess smooth plot
lineIntercept = c(quantile(p2$percent_active_days, c(.25, .50, .75)), 
                  mean(p2$percent_active_days))
f1_1 = ggplot(data_2_1_binned, aes(x=as.character(bins), y=percent)) + 
  geom_point(size = 2.5)+
  labs(x="",
       y="")+
  geom_smooth(se=FALSE, col = "#882608", size = 1.5, aes(group=1))+ 
  geom_vline(xintercept = lineIntercept[1]/0.05, 
             linetype = "longdash", color = "#005266", size = 1) +
  geom_vline(xintercept = lineIntercept[2]/0.05, 
             linetype = "solid", color = "#005266", size = 1) +
  geom_vline(xintercept = lineIntercept[3]/0.05, 
             linetype = "longdash", color = "#005266", size = 1) +
  geom_vline(xintercept = lineIntercept[4]/0.05, 
             linetype = "dotted", color = "#005266", size = 1) +
  ylim(0, 0.20) +
  guides(fill=FALSE) +
  theme(panel.background = element_rect(fill="#d5e4eb"), 
        plot.background = element_rect(fill="#d5e4eb"),
        panel.grid.major.y = element_line(size=1.0),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") 

ggsave("f1_1.pdf", f1_1, width=10, height=10)

# Data: 12 big domains
bb_list = split(bb_data, bb_data$domain_numeric)
bb_expanded_list = split(bb_data_expanded, bb_data_expanded$domain_numeric)
# Figure 3.1 %days, user-month, excluding gap months
data_3_1 = lapply(bb_list, function(x) loessTbl(x$percent_active_days))
data_3_1_binned = binnedDataPlot(data_3_1)

# get x-axis intercept for quantile lines for data frames in a list  
lineIntercept = function(data){
  y = lapply(data, function(x)c(quantile(x$percent_active_days, 
                                         c(.25, .50, .75)), 
                                mean(x$percent_active_days)))
  facet = names(y)
  li = as.data.frame(do.call("rbind", y))
  li$facet = facet
  names(li)[4] <- c("mean")
  return(li)
}

li = lineIntercept(bb_list)

f3_1 = ggplot(data_3_1_binned, aes(x=as.character(bins), y=percent)) + 
  geom_point(size = 2.5)+
  facet_wrap(~facet, ncol=3, as.table = TRUE) +
  labs(x="",
       y="")+
  geom_smooth(se=FALSE, col = "#882608", size = 1.5, aes(group=1))+ 
  ylim(0, 0.50) +
  guides(fill=FALSE) +
  theme(panel.background = element_rect(fill="#d5e4eb"), 
        plot.background = element_rect(fill="#d5e4eb"),
        panel.grid.major.y = element_line(size=1.0),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") 

f3_1 = 
  f3_1 + geom_vline(aes(xintercept = li[,1]/0.05), li,
                  linetype = "longdash", color = "#005266", size = 1) +
  geom_vline(aes(xintercept = li[,2]/0.05), li,
             linetype = "solid", color = "#005266", size = 1) +
  geom_vline(aes(xintercept = li[,3]/0.05), li,
             linetype = "longdash", color = "#005266", size = 1) +
  geom_vline(aes(xintercept = li[,4]/0.05), li,
             linetype = "dotted", color = "#005266", size = 1) 


# Data: First 6 quarters of all users that have been active for at least 18 months on CC
# Figure set 4: Quarterly plot of all data (excluding first 3 months)
qdata_gap_out = split(qdata, qdata$user_quarter_index)
qdata_gap_out = qdata_gap_out[1:6]

# figure 4.1 %days, user-month, excluding gap months
data_4_1 = lapply(qdata_gap_out, function(x)loessTbl(x$percent_active_days))
data_4_1_binned = binnedDataPlot(data_4_1)
data_4_1_binned$facet = factor(data_4_1_binned$facet, 
                               levels = c("Month 1-3", "Month 4-6",
                                          "Month 7-9", "Month 10-12",
                                          "Month 13-15", "Month 16-18"))

li = lineIntercept(qdata_gap_out)

# plot it
f4_1 = ggplot(data_4_1_binned, aes(x=bins, y=percent)) + 
  geom_point(size = 2.5)+
  facet_wrap(~facet, ncol=3, as.table = TRUE) +
  #          facet_grid(facet~.) +
  labs(x="",
       y="")+
  geom_smooth(se=FALSE, col = "#882608", size = 1.5, aes(group=1))+ 
  ylim(0, 0.25) + # upper limit is obtained from summary(sub_qd$pct)
  guides(fill=FALSE) +
  theme(panel.background = element_rect(fill="#d5e4eb"), 
        plot.background = element_rect(fill="#d5e4eb"),
        panel.grid.major.y = element_line(size=1.0),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") 

f4_1 + geom_vline(aes(xintercept = li[,1]/0.05), li,
                  linetype = "longdash", color = "#005266", size = 1) +
  geom_vline(aes(xintercept = li[,2]/0.05), li,
             linetype = "solid", color = "#005266", size = 1) +
  geom_vline(aes(xintercept = li[,3]/0.05), li,
             linetype = "longdash", color = "#005266", size = 1) +
  geom_vline(aes(xintercept = li[,4]/0.05), li,
             linetype = "dotted", color = "#005266", size = 1) 
