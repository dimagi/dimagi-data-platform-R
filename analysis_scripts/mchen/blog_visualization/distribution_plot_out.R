# density distribution 
source("data_prep.R")
source("density_funcs.R")

# density distribution for first 6 quarters
png("rbl_rm_6.png")
getDensityPlot(rbl_p2)
dev.off()

# subset 4 (12 big projects) 
pdf("big_bros_density.pdf",
    width=18,
    height=15)

op = par(mfrow = c(3,4), 
         bg = "#d5e4eb", 
         las = 1,
         mar = c(2,2,2,2),
         oma = c(2,2,4,2),
         bty = "n",
         cex = 1.2)

lapply(big_bros_ls, function(x) {
  getDensityPlot(x)
})
legend("topright",
       legend = c("Median", "Mean"), 
       lty = 1:2, 
       lwd=c(2.5, 2.5, col=rep("#005266")))

dev.off()
par(op)

# density distribution of crs
crs = filter(rbl_all, domain_numeric == 40)
pdf("crs_density.pdf")
op = par(bg = "#d5e4eb", bty="n", las=1)
getDensityPlot(crs)
legend("topright", 
       legend = c("Median", "Mean"), 
       lty = 1:2, 
       lwd=c(2.5, 2.5, col=rep("#005266")))
dev.off()
par(op)

# density distribution of all data (rebalanced)
pdf("rbl_all_data_density.pdf") 
op = par(bg = "#d5e4eb", bty="n")
getDensityPlot(rbl_all)
legend("topright", 
       legend = c("Median", "Mean"), 
       lty = 1:2, 
       lwd=c(2.5, 2.5, col=rep("#005266")))
dev.off()
par(op)


###################################################################
# Discrete Distribution
# Elements
# point(position(active days * active month), size(constant))
# vertical line(length(percentage of user-month))
###################################################################

# 12 big domains
pdf("big_bros_discrete.pdf",
    width=18,
    height=15)
op = ps(3,4,"#c2d6ef")
lapply(big_bros_ls, function(x){
  tbl = getDiscreteTbl(x)
  getDiscretePlot(tbl, max(tbl$pct))
})
dev.off()
par(op)

# quarterly final plot
q = tbl_df(as.data.frame(table(rbl_qdata$active_days, rbl_qdata$user_quarter_index)))
names(q) = c("active_days", "quarters", "count")
q = filter(q, count > 0)
q$active_days = as.numeric(q$active_days)
q$pct = q$count/sum(q$count)

quarterly = ggplot(q, aes(x=active_days, y=pct))+
  geom_point()+
  labs(x="", y="")+
  geom_smooth(se=FALSE, col = "#882608", size = 2)+ 
  scale_x_continuous(breaks = seq(1, 30, 2),name="") +
  facet_grid(quarters~.) +
  guides(fill=FALSE) +
  theme(plot.title = element_text(size=rel(1.5),colour="black"),  
        panel.background = element_rect(fill="#d5e4eb"), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") 
ggsave("quarterly_3.pdf", quarterly, width=7,height=14,dpi=200)

# get 25%, 75% numbers for each quarter (post-processing in AI)
q_tbl_split = split(q, q$quarters)
q_tbl_lines = lapply(q_tbl_split, function(x) percentileLines(x))
q_lines = q_tbl_lines[1:6]


# big brother final
bb_tbl_list = lapply(bb_list, function(x){
  y = tbl_df(as.data.frame(table(x$active_days, x$domain_numeric)))
  names(y) = c("active_days", "domain_numeric", "count")
  y = filter(y, count > 0)
  y$active_days = as.numeric(y$active_days)
  y$pct = y$count/sum(y$count)
  return(y)
})

bb_tbl = do.call(rbind, bb_tbl_list)
max_pct = bb_tbl %>%
  group_by(domain_numeric) %>%
  summarise(mpct = max(pct)) %>%
  arrange(., desc(mpct))
max_pct$dm_reorder = seq_len(nrow(max_pct))
reordered_dm = select(max_pct, domain_numeric, dm_reorder)
bb_tbl = left_join(bb_tbl, reordered_dm)

bbly = ggplot(bb_tbl, aes(x=active_days, y=pct))+
  geom_point()+
  labs(x="",
       y="")+
  geom_smooth(se=FALSE, col = "#005266", size = 1.5)+ 
  scale_x_continuous(breaks = seq(1, 30, 2)) +
  facet_wrap(~dm_reorder, ncol=3, as.table = TRUE) +
  guides(fill=FALSE) +
  theme(strip.background = element_rect(fill="#b4d8e7"),
        strip.text = element_text(face="bold",colour="black",hjust=0),
        panel.background = element_rect(fill="#d5e4eb"), 
        plot.background = element_rect(fill="#d5e4eb"),
        panel.grid.major.y = element_line(size=1.0),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") 

ggsave("big_domain_final.pdf", bbly, width=12,height=12,dpi=200)

# get 25%, 75%, median, mean for post-processing in AI
bb_tbl_lines = lapply(bb_list, function(x) summary(x$active_days))
q_tbl_lines = lapply(qdata_split,  function(x) summary(x$active_days))

# single plot: all data (balanced)
all_tbl = getDiscreteTbl(rbl_all)
all_lines = percentileLines(all_tbl)
all = ggplot(all_tbl, aes(x=active_days, y=pct))+
  geom_point()+
  labs(x="",
       y="")+
  geom_smooth(se=FALSE, col = "#882608", size = 1.5)+ 
  scale_x_continuous(breaks = seq(1, 30, 2)) +
  guides(fill=FALSE) +
  theme(panel.background = element_rect(fill="#d5e4eb"), 
        plot.background = element_rect(fill="#d5e4eb"),
        panel.grid.major.y = element_line(size=1.0),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") 
ggsave("all_final.pdf", all, width=7,height=7)

# single plot: crs
p2_tbl = getDiscreteTbl(rbl_p2)
p2_lines = summary(rbl_p2$active_days)
crs = ggplot(p2_tbl, aes(x=active_days, y=pct))+
  geom_point()+
  labs(x="",
       y="")+
  geom_smooth(se=FALSE, col = "#882608", size = 1.5)+ 
  scale_x_continuous(breaks = seq(1, 30, 2)) +
  scale_y_continuous(limits=c(0,0.25))+
  guides(fill=FALSE) +
  theme(panel.background = element_rect(fill="#d5e4eb"), 
        plot.background = element_rect(fill="#d5e4eb"),
        panel.grid.major.y = element_line(size=1.0),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") 
ggsave("all_final.pdf", all, width=7,height=7)

# quarterly distribution data export
qtr_data = lapply(rbl_qdata_split, function(x){
  y = getDiscreteTbl(x)
  return(y)
})
# merge all quarterly aggregate data
colNames = names(qtr_data)
for (i in seq_len(length(qtr_data))) {
  names(qtr_data[[i]]) = c("active_days",
                           paste("count",colNames[i],sep="_"),
                           paste("pct",colNames[i],sep="_"))
}
qtr_data_merged = Reduce(function(...) merge(..., all=TRUE), qtr_data)
qtr_data_merged[is.na(qtr_data_merged)] <- 0
write.csv(qtr_data_merged, "data_table_qtr.csv")
