# plot functions
plotOut <- function(data, medianColor, palette1, palette2, xmax, ymax, rLabel, rSquareX, rSquareY) {
  p <- regression_plot(ncases_touched ~ prev_ncases_touched, data, 
                       shade=FALSE,
                       spag=TRUE, 
                       median.col=medianColor, # median line (not regression line)
                       palette=colorRampPalette(c(palette1,palette2))(4))
  
  p <- p + 
    scale_x_continuous("Cases visited in month N-1", limits=c(0,xmax)) +
    scale_y_continuous("Cases visited in month N", limits=c(0,ymax)) # modify the axis limits
  
  #  p <- p + labs(x="Cases visited in month N-1", y="Cases visited in month N") # modify axis labels
  
  # adding annotation layer
  p <- p + annotate("text", x=rSquareX, y=rSquareY, size=7, label=rLabel, colour="red", parse=T) 
  # modify theme settings:
  p <- p + 
    theme(axis.title.x=element_text(vjust = -1.5,size=rel(2)),
          axis.title.y=element_text(vjust = 1.5,size=rel(2)),
          plot.margin=unit(c(1,1,1,1),"cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(aspect.ratio = 1) +
    theme(legend.position="none") 
  return(p)
}

