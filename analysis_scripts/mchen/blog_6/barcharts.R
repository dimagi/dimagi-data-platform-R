# barcharts
library(readxl)
f1 = read_excel("figures.xlsx", "f1")
f2 = read_excel("figures.xlsx", "f2")
f3 = read_excel("figures.xlsx", "f3")

names(f1) = c("active_months","active_q4","inactive_q4")
wide_f1 = reshape(f1, idvar="active_months",varying=c("active_q4","inactive_q4"),
                  v.names="number",direction="long")
names(wide_f1)[2] = c("status")
wide_f1$status = as.factor(wide_f1$status) # position_dodge would not work otherwise
wide_f1$active_months = factor(wide_f1$active_months, 
                               levels(wide_f1$active_months)[c(1,4,7,9,2,3,5,6,8)])  

library(ggplot2)
library(grid)

# SCALES
# dim(1): active months on CC
# dim(2): number of programs (active/inactive)
# ELEMENTS: bar
maxnum = max(wide_f1$number)
minnum = min(wide_f1$number)
ggplot(wide_f1, aes(x=active_months, 
                    y=number,
                    fill=status)) + 
  geom_bar(width=0.65, position="dodge", stat="identity") +
  scale_y_continuous(breaks = seq(minnum, maxnum, by=10))+
  scale_fill_manual(values=c("#01526d","#00a4dc"),
                    name="",
                    labels=c("Active in Q4 2014 (ongoing programs)",
                             "Not active in Q4 2014")) +
  labs(x = "Active months on CommCare",
       y = "No. of programs") +
  theme(legend.position = "top",
        legend.background = element_rect(fill = "transparent"),
        axis.ticks = element_blank(),
        axis.ticks.margin = unit(0.25, "cm"),
        axis.text.x = element_text(face="bold",colour="black"),
        axis.text.y = element_text(face="bold",colour="black"),
        panel.background = element_rect(fill = "#d5e2e8"),
        plot.background = element_rect(fill = "#d5e2e8"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()
  )
ggsave("figure_1.pdf",width=8,height=8)  

# chart 2: months on CC
names(f2) = c("months", "total", "active")
f2$months = as.factor(f2$months)
f2$months = factor(f2$months, levels(f2$months)[c(1,4,7,9,2,3,5,6,8)])
f2_1 = ggplot(subset(f2, select = c(months, total)), aes(x=months, y=total))+
  geom_bar(width=0.6, stat="identity", fill="#01526d") + 
  labs(x="",
       y="")+
  theme(legend.position = "top",
        legend.background = element_rect(fill = "transparent"),
        axis.ticks = element_blank(),
        axis.ticks.margin = unit(0.25, "cm"),
        axis.text.x = element_text(face="bold",colour="black",angle=90),
        axis.text.y = element_text(face="bold",colour="black",angle=90),
        panel.background = element_rect(fill = "#d5e2e8"),
        plot.background = element_rect(fill = "#d5e2e8"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()
  ) + coord_flip() 

f2_2 = ggplot(subset(f2, select = c(months, active)), aes(x=months, y=active))+
  geom_bar(width=0.6, stat="identity", fill="#74d0f5") + 
  labs(x="",
       y="")+
  theme(legend.position = "top",
        legend.background = element_rect(fill = "transparent"),
        axis.ticks = element_blank(),
        axis.ticks.margin = unit(0.25, "cm"),
        axis.text.x = element_text(face="bold",colour="black",angle=90),
        axis.text.y = element_text(face="bold",colour="black",angle=90),
        panel.background = element_rect(fill = "#d5e2e8"),
        plot.background = element_rect(fill = "#d5e2e8"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()
  ) + coord_flip()

# dodged barcharts for comparison
# reshaping data: wide to long
library(reshape)
f2_reshaped = reshape(f2, idvar="months", varying=list(2:3), # put list(3:2) here and you would see interesting result
                      v.names="status", direction="long")
ggplot(f2_reshaped, aes(x=months, y=status, fill=factor(time))) +
  geom_bar(width=0.6, stat="identity", position="dodge") +
  labs(x="",
       y="")+
  theme(legend.position = "top",
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.margin = unit(0.25, "cm"),
        axis.text.x = element_text(face="bold",colour="black"),
        axis.text.y = element_text(face="bold",colour="black"),
        panel.background = element_rect(fill = "#d5e2e8"),
        plot.background = element_rect(fill = "#d5e2e8"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) + 
  coord_flip()  

# chart 3: re-start rate
names(f3) = c("month", "attrited", "restarted", "pct")
f3$month = as.factor(f3$month)
f3$month = factor(f3$month, levels(f3$month)[c(1,4,5,6,2,3)])

maxpct = max(f3$pct)
minpct = min(f3$pct)
ggplot(f3, aes(x=month,y=pct)) +
  geom_bar(width=0.6, stat="identity", fill="#01526d") +
  scale_y_continuous(breaks = seq(0, maxnum, by = 0.10)) + 
  labs(x="",
       y="") + 
  theme(legend.position = "top",
        legend.background = element_rect(fill = "transparent"),
        axis.ticks = element_blank(),
        axis.ticks.margin = unit(0.25, "cm"),
        axis.text.x = element_text(face="bold",colour="black"),
        axis.text.y = element_text(face="bold",colour="black"),
        panel.background = element_rect(fill = "#d5e2e8"),
        plot.background = element_rect(fill = "#d5e2e8"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()
  )
ggsave("figure_3.pdf",width=8,height=8)  