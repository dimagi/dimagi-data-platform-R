# multiple base density plots
library(KernSmooth)

# get quartile information to-be-plotted
getQuantiles = function(x) {
  dens = bkde(x$active_days,bandwidth=0.25)
  lower.v = quantile(x$active_days,probs=0.25)
  median.v = median(x$active_days) 
  upper.v = quantile(x$active_days,probs=0.75)
  mean.v = mean(x$active_days)
  min.v = min(dens$x) 
  max.v = max(dens$x)  
  dPlot = c(dens, lower.v, median.v, upper.v, mean.v, min.v, max.v) 
  names(dPlot) = c("x", "y",  # x and y coordinates from bkde() function
                   "lower.v", "median.v", "upper.v", "mean.v", "min.v", "max.v")
  return(dPlot)
}


##########################################################################
# Density Distribution Plot 
# Elements
# smooth.density.kernel.estimate(user-month distribution)
# vertical line(median, mean of active days)
# shaded area (25% quartile, 75% quartile of user-month distribution)
##########################################################################
getDensityPlot = function(data) {
  with(data, {
    d_data = getQuantiles(data)
    coord = d_data[c("x","y")]
    plot(coord,  type = "l", lwd = 2,
         axes = FALSE,
         xlab = "", ylab = "",
         col = "#004865",
         xaxt = "n",
         xaxs = "i",
         xlim = c(1,30))
    axis(1, xaxp = c(1,25,8), tck = -0.01)
    axis(2, yaxp = c(0.00, round(max(coord$y),digits=2), 5), tck = -0.01)
    
    abline(v =d_data[["median.v"]], col = "#004865", lty = 1, lwd = 2)  
    abline(v =d_data[["mean.v"]], col = "#004865", lty = 2, lwd = 2)
    
    polygon(c(d_data[["upper.v"]], 
              coord$x[coord$x > d_data[["upper.v"]] & 
                        coord$x < d_data[["max.v"]]], 
              d_data[["max.v"]]), 
            c(0, coord$y[coord$x> d_data[["upper.v"]] & 
                           coord$x < d_data[["max.v"]]], 0),
            col="#004865")
    
    polygon(c(d_data[["min.v"]], 
              coord$x[coord$x > d_data[["min.v"]] & 
                        coord$x < d_data[["lower.v"]]], d_data[["lower.v"]]),
            c(0, coord$y[coord$x > d_data[["min.v"]] & 
                           coord$x < d_data[["lower.v"]]], 0),
            col="#004865")
  })
}

# parameter setting
ps = function(N1, N2, background){
  x = par(mfrow = c(N1,N2), 
          bg = background, 
          #          mar = c(2,2,2,2),
          oma = c(1,1,3,1),
          bty = "n")
  return(x)
}

# tabulate data 
getDiscreteTbl = function(x) {
  y = tbl_df(as.data.frame(table(x$active_days)))
  names(y) = c("active_days", "count")
  y$pct = y$count/sum(y$count)
  y$active_days = as.numeric(y$active_days)
  return(y)
}

# this returns the exact x-coordinate for 25 percentile, 75 percentile (to be used in geom_vline)
getPercentiles = function(x) {
  smr = summary(x$active_days) # get summary stats 
  percentiles = smr[2:5] # exclude min/max. we do not plot those two vertical lines
  names(percentiles) = c("p25", "median", "mean", "p75")
  return(percentiles)
}


############################################################
# final distribution plot
# Elements
# point(position(active days * percent of user-month))
# a smoother
# vertical lines (mean, median)
############################################################

# quarterly final plot
q = tbl_df(as.data.frame(table(rbl_qdata$active_days, rbl_qdata$user_quarter_index)))
names(q) = c("active_days", "quarters", "count")
q = filter(q, count > 0)
q$active_days = as.numeric(q$active_days)
q$pct = q$count/sum(q$count)


###############
# single plot #
###############
# Note ggplot can only operate in global env
single =  ggplot(tbl, aes(x=active_days, y=pct)) +
  geom_point() +
  labs(x="", y="") +
  geom_smooth(se=FALSE, col = "#882608", size = 2) + 
  scale_x_continuous(breaks = seq(1, 30, 2),name="") +
  scale_y_continuous(limits = c(0, 0.25)) +
  guides(fill=FALSE) + 
  theme(plot.title = element_text(size=rel(1.5),colour="black"),  
        panel.background = element_rect(fill="#d5e4eb"), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")       

################### 
# multiple facets #
###################
# use domain_numeric as a facetting factor for big domain viz
bb_tbl_ls = lapply(bb_list, function(x) {
  y = getDiscreteTbl(x)
})
bb_tbl_ls = lapply(seq_along(bb_tbl_ls), function(i) {
  bb_tbl_ls[[i]]$domain_numeric = rep(names(bb_tbl_ls)[[i]], nrow(bb_tbl_ls[[i]]))
  return(bb_tbl_ls[[i]])
})
bb_tbl = do.call(rbind, bb_tbl_ls)
bb_tbl$domain_numeric = as.factor(bb_tbl$domain_numeric)

# put vline data in a separate data frame
bb_vline = lapply(bb_list, function(x) getPercentiles(x))
bb_vline = do.call(rbind, bb_vline)
bb_vline = as.data.frame(bb_vline)
bb_vline$domain_numeric = factor(dimnames(bb_vline)[[1]])

multiple = ggplot(bb_tbl, aes(x=active_days, y=pct)) +
  geom_point() +
  labs(x="", y="") +
  geom_smooth(se=FALSE, col = "#882608", size = 2) + 
  scale_x_continuous(breaks = seq(1, 30, 2)) +
  scale_y_continuous(limits = c(0, 0.25)) +
  guides(fill=FALSE) +
  facet_wrap(~domain_numeric) +
  theme(plot.title = element_text(size=rel(1.5),colour="black"),  
        panel.background = element_rect(fill="#d5e4eb"), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  # add different vertical lines to each facet  
  geom_vline(aes(xintercept = p25, colour = "#ed7d31"), bb_vline) +
  geom_vline(aes(xintercept = p75, colour = "#5d9cd6"), bb_vline) +
  geom_vline(aes(xintercept = median, colour = "#ffbf01"), bb_vline) +
  geom_vline(aes(xintercept = mean, colour = "#ed7d31"), bb_vline)
