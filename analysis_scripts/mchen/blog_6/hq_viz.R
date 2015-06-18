# IMPORT DATA #
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(zoo))
suppressMessages(library(lubridate))

data = tbl_df(read.csv("blog_data_6_10_15.csv", stringsAsFactors=FALSE))
data$calendar_month = as.Date(data$calendar_month, "%Y-%m-%d") # time data in DP is stored as numeric distance from 1970-01-01

hq = select(data, domain_numeric, user_pk, calendar_month) # keep columns of interest
hq = arrange(hq, domain_numeric, user_pk, calendar_month)
t = filter(hq, calendar_month >= as.Date("2012-01-01")) %>% 
  filter(., calendar_month <= as.Date("2014-12-01")) # keeping data from 2012 onward


# for each domain, get total number of users and number of active months 
agg = t %>%
  group_by(domain_numeric) %>% # change from domain_numeric to domain
  summarise(nusers=n_distinct(user_pk),
            nmonths = n_distinct(calendar_month))

# for each domain in each month, get the number of active users. TBF'ED
t_agg_1 = t %>%
  group_by(domain_numeric, calendar_month) %>%
  summarise(nusers = n_distinct(user_pk)) %>%
  arrange(., domain_numeric, calendar_month)

# for each domain, get total user blocks
t_agg_1$blocks = round_any(t_agg_1$nusers, 25, ceiling)/25

# rank domains by max user blocks in a given month (big domains go to the bottom)
d_rank = t_agg_1 %>% 
  group_by(domain_numeric) %>%
  summarise(b = max(blocks)) %>%
  arrange(., desc(b)) %>%
  mutate(b_rank = seq(b)) %>%
  select(., domain_numeric, b_rank)

# Replicate each row; set the number of replications to the number of blocks
expanded = as.numeric(rep(row.names(t_agg_1), t_agg_1$blocks))
texp = t_agg_1[expanded,] 
texp = texp[order(match(texp$domain_numeric, d_rank$domain_numeric)),] # order domains by max active month
texp = left_join(texp, d_rank)
texp = texp %>%
  group_by(b_rank, calendar_month) %>%
  mutate(incre_blocks = seq(n())) # Index user bins incrementally

# for each cum_blocks value, sum up cum_blocks and the max value of the domain that's sitting below
piles = texp %>%
  group_by(b_rank) %>%
  summarise(max_blocks = max(incre_blocks))
piles$max_blocks = c(0, cumsum(piles$max_blocks[-nrow(piles)])) 
texp = left_join(texp, piles) 
texp$piled_blocks = texp$incre_blocks + texp$max_blocks # building blocks into mounds for each domain


# all HQ domains
# geom_tile plot 
suppressMessages(library(ggplot2))
suppressMessages(library(scales))
suppressMessages(library(RColorBrewer))

x_axis_ticks = as.character(seq(min(texp$calendar_month),max(texp$calendar_month),by="quarter")) # R considers Date variable as continuous
x_axis_labels = format(as.Date(x_axis_ticks), "%b %y")  

g0 = ggplot(texp, aes(x=as.character(calendar_month),y=piled_blocks)) + 
  geom_tile(aes(fill=factor(texp$b_rank))) + 
  guides(fill=FALSE) + # turning off legends
  scale_fill_manual(values=rep(c("#005266", "#882608", "#fe9c7b"),
                               ceiling(n_distinct(texp$domain_numeric)/3))) + # repeating colors to distinguish each domain
  scale_y_continuous(breaks = NULL) + # suppressing ticks, gridlines
  scale_x_discrete(breaks = x_axis_ticks, labels = x_axis_labels) + # reduce total number of breaks
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), # hide titles for both x- and y-axis
        axis.text.x = element_text(face="bold",
                                   colour="#004865", size=11),
        panel.background = element_rect(fill="#d5e4eb"),
        panel.grid = element_blank())  

ggsave("hq_viz.pdf", g0, width = 8, height = 12)


# subset HQ domains 
# subs = c(11, 67, 87, 127, 138)
# run line 17-29 and you will get the data
t = filter(t, domain_numeric %in% subs)
hq_sub = ggplot(t_agg_1, aes(x=calendar_month, y=nusers)) +
  geom_bar(width=28,stat="identity") +
  facet_wrap(~domain_numeric, nrow=5, scales = "free_y") + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), # hide titles for both x- and y-axis
        axis.text.x = element_text(face="bold",
                                   colour="#004865", size=11),
        panel.background = element_rect(fill="#d5e4eb"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

# highlight time range
rect = data.frame(xmin = as.Date(c("2012-04-01","2012-10-01","2013-04-01","2013-10-01","2014-04-01","2014-10-01")), 
                  xmax = as.Date(c("2012-06-01","2012-12-01","2013-06-01","2013-12-01","2014-06-01","2014-10-01")),
                  ymin = c(-Inf,-Inf,-Inf,-Inf,-Inf,-Inf),
                  ymax = c(Inf,Inf,Inf,Inf,Inf,Inf))
hq_sub_2 = hq_sub + geom_rect(data = rect, aes(xmin=xmin, xmax=xmax,
                                      ymin = ymin, ymax=ymax),
                     fill = "#aaaea6",
                     alpha = 0.3,
                     inherit.aes = FALSE) 

ggsave("hq_subdomains.pdf", hq_sub_2, width = 8, height = 8)
