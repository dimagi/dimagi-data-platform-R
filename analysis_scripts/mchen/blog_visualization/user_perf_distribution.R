library(plyr)
library(dplyr)
library(lattice)
library(latticeExtra)

data = tbl_df(read.csv("blog.csv", stringsAsFactors=FALSE))

# SUBSET COLUMNS OF INTEREST (not sure )
all = select(data, domain_numeric, user_pk, user_id, active_days, ncases_touched, calendar_month)
all$calendar_month = as.Date(all$calendar_month)
all = filter(all, calendar_month >= as.Date("2010-01-01")) 
all = filter(all, calendar_month <= as.Date("2015-04-01"))
n_distinct(all$domain_numeric); n_distinct(all$user_pk); n_distinct(all$calendar_month); min(all$calendar_month); max(all$calendar_month)

# CREATE CUSTOM QUARTERS BY USER (METHOD#2: EXCLUDING INACTIVE MONTHS)
all = arrange(all, domain_numeric, user_pk, calendar_month)
all = all %>%
  group_by (domain_numeric, user_pk) %>%
  mutate(user_month_index = seq(n())) # indexing each user-month

# CUT USER MONTHS INTO CUSTOM QUARTERS (IMPLEMENT METHOD 2)
bins = c(1+(3*(0:ceiling(max(all$user_month_index)/3))))
labs = paste("Q", seq(length(bins)-1), sep = "")
all = all %>% 
  group_by(domain_numeric, user_pk) %>%
  mutate(user_quarter_index = cut(user_month_index,
                                  breaks = bins,
                                  labels = labs,
                                  right = FALSE))

# ALL DATA
all_naOmit = na.omit(all)
pdf(7, 14, file="allData.pdf", useDingbats=FALSE)
png(filename="allData.png")

p_all = densityplot(~active_days, data=all_naOmit, 
                    panel=function(x,...){
                      panel.densityplot(x,...)
                      lower.v <- quantile(x,probs=0.25)
                      median.v <- median(x) 
                      upper.v <- quantile(x,probs=0.75)
                      mean.v <- mean(x)
                      
                      panel.abline(v=median.v, col.line="#003d71", lwd=1.2)  # 50 percentile
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
                    scales=list(x=list(at=c(1,2,3,4,5,6,7,8,9,10,20,30)),
                                alternating = 1),
                    xlim=range(all_naOmit$active_days),
                    ylim=c(0,0.15),
                    xlab="Active days per month",
                    ylab="Density",
                    col="#413f3f",
                    plot.points=FALSE) 
print(p_all)
dev.off()


# SUBSETS OF INTEREST
# flagship project in India
crs = filter(all, domain_numeric == 40)
crs_qtr_1 = filter(crs, user_quarter_index == "Q1")
crs_qtr_1 = na.omit(crs_qtr_1)

pdf(7, 14, file="crs_remind_quarter_1.pdf", useDingbats=FALSE)
png(filename="crs_remind_quarter_1.png")

p_crs_q1 = densityplot(~active_days, data=crs_qtr_1, 
                       panel=function(x,...){
                         panel.densityplot(x,...)
                         lower.v <- quantile(x,probs=0.25)
                         median.v <- median(x) 
                         upper.v <- quantile(x,probs=0.75)
                         mean.v <- mean(x)
                         
                         panel.abline(v=median.v, col.line="#003d71", lwd=1.2)  # 50 percentile
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
                       scales=list(x=list(at=c(1,2,3,4,5,6,7,8,9,10,20,30)),
                                   alternating = 1),
                       xlim=range(crs_qtr_1$active_days),
                       ylim=c(0,0.25),
                       xlab="Active days per month",
                       ylab="Density",
                       col="#413f3f",
                       plot.points=FALSE) 

print(p_crs_q1)
dev.off()

# big bromance: top 12 projects in certain month
bigBros = function(data, N) {
  data %>%
    group_by(domain_numeric, calendar_month) %>%
    summarise(tot_users = n_distinct(user_pk)) %>%
    summarise(max_users = max(tot_users)) %>%
    arrange(., desc(max_users)) %>%
    top_n(N)
}

big_12 = bigBros(all_naOmit, 12)  # may include more than 12 if there are ties
if(nrow(big_12) > 12) big_12 = big_12[1:12,] 
big_12$rank = c(1:12)

# subset data of these 12 domains
big_12_domains = big_12$domain_numeric
big_bros = left_join(big_12, all)

png(filename = "big_12_domains.png",
    width = 1080,
    height = 1080,
    bg = "transparent")

big_bros$user_quarter_index = factor(big_bros$user_quarter_index)
big_bros$rank = factor(big_bros$rank)

pdf(14, 14, file = "p_big_bros.pdf")
myUsers = big_12$max_users
myMonths =  big_bros %>%
  group_by(rank) %>%
  summarise(nmonths = n_distinct(calendar_month)) %>%
  select(., nmonths)

p_big_bros = densityplot(~active_days|rank, data=big_bros_naOmit,
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
                           #                     panel.text(25,0.20,labels=paste("Users: ", myUsers[which.packet(),], sep = ""))
                           #	                   panel.text(25,0.17,labels=paste("Months: ", myMonths[which.packet(),], sep = ""))
                         },
                         #	                 par.settings = theEconomist.theme(),
                         #	                 scales = "free",	
                         scales=list(x=list(at=c(1,2,3,4,5,6,7,8,9,10,20,30)),
                                     alternating = 1),
                         xlim=range(big_bros_naOmit$active_days),
                         ylim=c(0,0.25),
                         xlab="Active days per month",
                         ylab="Density",
                         #                 aspect=0.1,
                         layout=c(3,4),
                         as.table=TRUE, # The flag "as.table=TRUE" changes to left to right and top to bottom
                         strip=strip.custom(bg = "#ffffff", 
                                            par.strip.text = list(font=0.5,col="#413f3f"),
                                            factor.levels = levels(big_bros_naOmit$rank)),
                         col="#413f3f",
                         plot.points=FALSE)	
print(p_big_bros)
dev.off()


# from each project, select users that have been active for at least N quarters
N = 6
qtr_n = all_naOmit %>% 
  group_by(domain_numeric, user_pk) %>%
  filter(., max(user_month_index) >= N*3) %>%
  filter(., user_month_index <= N*3)
qtr_n$user_quarter_index = factor(qtr_n$user_quarter_index)
# only visualize N quarters (no more than N as we want every quarter on the graph is contributed by the same set of users)
viz_filename = paste("quarters_", N, sep = "")
pdf(7,14, file = paste(viz_filename, "pdf",sep = "."))
png(filename = paste(viz_filename, "png",sep = "."), 
    width = 720,
    height = 840,
    bg = "transparent")
qtr_n_viz = distributionViz(qtr_n$active_days, qtr_n$user_quarter_index, qtr_n)
print(qtr_n_viz)
dev.off()


# ADD VERTICAL LINES TO DENSITY PLOTS
addLine<- function(a=NULL, b=NULL, v = NULL, h = NULL, ..., once=F) {
  tcL<- trellis.currentLayout()
  k<-0
  for(i in 1:nrow(tcL))
    for(j in 1:ncol(tcL))
      if (tcL[i,j]>  0) {
        k<-k+1
        trellis.focus("panel", j, i, highlight = FALSE)
        if (once) panel.abline(a=a[k], b=b[k], v=v[k], h=h[k], ...) else
          panel.abline(a=a, b=b, v=v, h=h, ...)
        trellis.unfocus()
      }
}


# DISTRIBUTION FUNCTION
distributionViz = function(var, groups, data){
  densityplot(~var|groups, data=data,
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
                #	                   panel.text(0,0, labels = myText[panel.number()])
              },
              scales=list(x=list(at=c(1,2,3,4,5,6,7,8,9,10,20,30))),
              xlim=range(var), # reversing the order of x-axis values
              ylim=c(0,0.15),
              xlab="Active days per month",
              ylab="Density",
              #                 aspect=0.1,
              layout=c(1,length(levels(groups))),
              index.cond=list(c(length(levels(groups)):1)),
              strip=strip.custom(bg="#ffffff", par.strip.text=list(font=0.5,col="#413f3f")),
              col="#413f3f",
              plot.points=FALSE)	
}


# DISTRIBUTION VIZ: ALL DATA
lower.v = quantile(all$active_days,probs=0.25)
lower.v <- quantile(all$active_days,probs=0.25)
median.v <- median(all$active_days) 
upper.v <- quantile(all$active_days,probs=0.75)
mean.v <- mean(all$active_days)
dens <- density(all$active_days)                   
min.v <- min(dens$x)
max.v <- max(dens$x)

#png(filename = "distAllData.png",
#	bg = "#fff0b4")
distAll = densityplot(~active_days, data=all,
                      scales=list(x=list(at=c(1,2,3,4,5,6,7,8,9,10,20,30))),
                      xlim=range(all$active_days), # reversing the order of x-axis values
                      ylim=c(0,0.25),
                      xlab="Active days per month",
                      ylab="Density",
                      #                  col="#f58220",
                      #                  lwd=2.5,
                      par.settings = theEconomist.theme(with.bg = TRUE, box = "transparent"),
                      plot.points=FALSE                  
)

distAll = distAll + 
  layer(
    panel.polygon(c(upper.v, dens$x[dens$x > upper.v & dens$x < max.v], max.v), 
                  c(0, dens$y[dens$x> upper.v & dens$x < max.v], 0),
                  col="#f58220")) + 
  layer(
    panel.polygon(c(min.v, dens$x[dens$x > min.v & dens$x < lower.v], lower.v),
                  c(0, dens$y[dens$x > min.v & dens$x < lower.v], 0),
                  col="#f58220")) +
  layer(
    panel.abline(v=median.v, col.line="#003d71", lwd=1.2)) +
  layer(
    panel.abline(v=mean.v, col.line="#003d71", lwd=1.2, lty=2)) 
print(distAll)
dev.copy(pdf, "distAll_2.pdf", bg = "#fff0b4") #  copy the graph already on the screen graphics device to a PDF
dev.off()