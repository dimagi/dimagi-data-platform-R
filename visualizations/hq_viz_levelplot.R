########################################
# using levelplot for hq visualization #
########################################

library(lattice)
library(zoo)
# create a matrix using domain_numeric, calendar_month 
ldata = select(data, domain_numeric, calendar_month)

lmatrix = matrix(0,
                 nrow=length(unique(ldata$domain_numeric)),
                 ncol=length(unique(ldata$calendar_month))) # create an empty matrix
rownames(lmatrix) = paste("domain_", unique(ldata$domain_numeric), sep="")
colnames(lmatrix) = as.character(sort(unique(ldata$calendar_month)))

# split ldata into a list of data frames by domain_numeric
ldata = split(ldata, ldata$domain_numeric)

# assign values to each cell in the matrix
for (i in seq(nrow(lmatrix))) {  
  lmatrix[i,] = as.numeric(as.Date(colnames(lmatrix)) %in% 
                             unique(ldata[[i]]$calendar_month))  
}

# replace 0 with NA
lmatrix[lmatrix == 0] <- NA

levelplot(t(lmatrix), 
          colorkey = FALSE,
          scales = list(x = list(rot = 90)),
          panel = function(x, y, z,...) {
            panel.abline(v = unique(as.numeric(x)), 
                         h = unique(as.numeric(y)), 
                         col = "darkgrey")
            panel.xyplot(x, y, pch = 16 * z, ...)
          },
          xlab = "calendar month")
