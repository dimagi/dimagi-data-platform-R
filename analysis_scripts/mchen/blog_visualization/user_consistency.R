#####################################################################
# visualization code developed for Dimagi's data blog series  
# Dec 17, 2014
#####################################################################

library(grid)
library(gridExtra)
library(ggplot2)
library(sorvi)
library(dplyr)
library(RColorBrewer)





############################################ 
# domain consistency comparison plot
############################################

# side-by-side plot of two demo domains: domain 18 and 264
# data source: domain_consistency_comparison.csv
data <- read.table("domain_consistency_comparison.csv",header=T,sep=",")
dm1 <- data %>% filter(data$domain_numeric == 18)
dm2 <- data %>% filter(data$domain_numeric == 264)

source("plot_funcs.R")
plot1 <- plotOut(dm1,"red","#67a9cf","#ef8a62",50,50,paste("r^2==",0.75),5,45)
plot2 <- plotOut(dm2,"red","#67a9cf","#ef8a62",50,50,paste("r^2==",0.36),5,45)

# output two plots side-by-side 
# format: pdf
pdf(file="domain_comparison.pdf",
    width=14,
    height=10)
# png("domain_comparison.png", width = 1600, height = 1600, units = "px", pointsize = 16,bg = "transparent", res=150)
grid.arrange(plot1, plot2, ncol = 2)
dev.off()





############################################ 
# top10 vs. bottom10 comparison plot
############################################
top_10p <- read.table("top_10p.csv",header=T,sep=",")
bottom_10p <- read.table("bottom_10p.csv",header=T,sep=",")
plot3 <- plotOut(top_10p, "red", "#67a9cf","#ef8a62",100,100,paste("r^2==",0.69),10,90)
plot4 <- plotOut(bottom_10p, "red", "#67a9cf","#ef8a62",100,100,paste("r^2==",0.34),10,90)
pdf(file="top_bottom_comparison.pdf",
    width=14,
    height=10)
grid.arrange(plot3, plot4, ncol = 2)
dev.off()





######################
# density plot
######################
# main plot: March only data
# exclude users who visit more than 50 cases 
# exclude users who visit only 1 case that month

test2 <- read.table("test2.csv",header=T,sep=",")
data <- test2 %>% filter(test2$x <= 50 & test2$x > 1)
mainPlotData<- data %>% filter(as.character(data$calendar_month) == "2014-03-01")
p <- ggplot(mainPlotData, aes(x = x, y = y)) + 
  geom_point(position=position_jitter(w=0.4,h=0.4),
             colour="black",
             alpha=0.7) + 
  xlim(1,50) + ylim(1,50)
p1 <- p + 
  stat_density2d(aes(fill = rev(..level..)), geom = "polygon")  
p2 <- p1 + 
  labs(x="Cases visited in March 2014", y = "Cases visited in April 2014") + 
  theme_bw() + 
  theme(legend.position="none") + 
  theme(aspect.ratio = 1) 
# increase label size, space between axis and labels
p3 <- p2 + theme(plot.margin = unit(c(1.5,1.5,1.5,1.5), "cm"),
                 axis.title.x = element_text(size=rel(1.5), vjust = -1.5),
                 axis.title.y = element_text(size=rel(1.5), vjust = 1.5)
)

ggsave("mainPlot.pdf", p3, dpi = 600, scale = 1.5, width = 4, height = 4)





######################
# Line graph
######################
attrition <- read.table("leadup.csv", header = T, sep = ",")
month_levels <- rev(levels(attrition$X1))

l1 <- ggplot(attrition, aes(x = X1, y = X2, colour = att_duration, group = att_duration, linetype = att_duration)) +
  geom_point(shape = 15, size = 4.0, colour="peachpuff4") +
  geom_line(size = 1.5) + 
  scale_x_discrete("Month 'X' before attrition event",limits = month_levels) 

l2 <- l1 + 
  scale_y_continuous("Number of cases visited",limits=c(0,12),breaks=c(0,4,8,12)) + 
  theme(aspect.ratio=0.5,
#        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(colour = "white",size=1),
        panel.background=element_blank()) 

l3 <- l2 + theme(axis.title.x = element_text(size=rel(1.5), vjust = -1.5),
                 axis.title.y = element_text(size=rel(1.5), vjust = 1.5),
                 legend.title = element_blank())






##########################################
# user experience: stacked barplot
##########################################

# use colclasses to load data faster (http://www.r-bloggers.com/using-colclasses-to-load-data-more-quickly-in-r/)
ux_data <- read.csv("user_experience.csv", 
                    stringsAsFactors = FALSE, 
                    colClasses=c("integer","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                 "factor","factor","factor","factor"))
colnames(ux_data)[14:17] <- c("Q1 to Q2","Q2 to Q3", "Q3 to Q4", "Q1 to Q4")

# resize bins: 
# <-20%, 1
#[-20%,20%], 2
#[20%,50%],3
# >50%,4

binCut <- function(var, n1, n2, n3, n4){
  sapply(var, function(x) 
    if(x <= n1)                  {return("1")
    } else if(x > n1 & x <= n2)  {return("2")
    } else if (x > n2 & x <= n3) {return("3")
    } else if (x > n3 & x <= n4) {return("4")
    } else {return("5")
    })  
}

ux_data$Q1_to_Q2 <- binCut(ux_data$percent_change_q1_2, -50, -20, 20, 50)
ux_data$Q2_to_Q3 <- binCut(ux_data$percent_change_q2_3, -50, -20, 20, 50)
ux_data$Q3_to_Q4 <- binCut(ux_data$percent_change_q3_4, -50, -20, 20, 50)
ux_data$Q1_to_Q4 <- binCut(ux_data$percent_change_q1_4, -50, -20, 20, 50)

# reshape data: wide to long
l <- reshape(ux_data, 
             varying = c("Q1_to_Q2","Q2_to_Q3","Q3_to_Q4","Q1_to_Q4"),
             v.names = "percentage_change",
             timevar = "quarter",
             times = c("Q1_to_Q2","Q2_to_Q3","Q3_to_Q4","Q1_to_Q4"),
             direction = "long")

# change the order of levels of factor quarter for visualization
l$quarter <- factor(l$quarter, levels = c("Q1_to_Q2","Q2_to_Q3","Q3_to_Q4","Q1_to_Q4"))

p0 <- ggplot(l, aes(x=factor(quarter), fill=factor(percentage_change), y=4*100*(..count..)/sum(..count..)))
p1 <- p0 + geom_bar(width=0.7) +
  scale_fill_manual(values = c("#e34a33","#fdbb84","#e5e5ab","#91bfdb","#67a9cf"), 
                    breaks = c(1:5), 
                    labels = c("Substantial drop (>50%)",
                               "Moderate drop (20-50%)",
                               "Stable (+/- 20%)",
                               "Moderate increase (20-50%)",
                               "Substantial increase (>50%)")) 

##########################################################################
### control style (http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/)###
##########################################################################

# change title appearance
p2 <- p1 + 
  labs(x="Interval", y="Percent of Users") +
  scale_x_discrete(labels=c("Q1 to Q2",
                            "Q2 to Q3",
                            "Q3 to Q4",
                            "Q1 to Q4")) +
  theme(plot.title = element_text(face = "bold"))

# set title to twice the base font size
p3 <- p2 + theme(plot.title = element_text(size = rel(1.8)))

# change panel and plot attributes
p4 <- p3 + theme(panel.background = element_blank()) 

# change legend attributes
p5 <- p4 + theme(legend.text = element_text(size = 10),
                 legend.title = element_blank(),
                 legend.background = element_rect(colour = "black"),
                 legend.key.size = unit(0.75, "cm"),
                 axis.title.x = element_text(vjust = - 1.5, size = rel(1.35)),
                 axis.title.y = element_text(vjust = 1.5, size = rel(1.35)),
                 plot.margin = unit(c(1,1,1,1),"cm")) 

# reverse the legend order
p6 <- p5 + guides(fill = guide_legend(reverse=TRUE))

print(p6)
ggsave("version6.pdf", p6, dpi = 600)



