library("Hmisc")
# import data from somewhere
# all_monthly <- .... 
#Run beginning of monthly usage report
all_monthly$batch_entry_percent= (all_monthly$batch_entry_percent)*100
all_monthly$median_visit_duration = round(all_monthly$median_visit_duration/60,
                                          digits=2) 
# monthly indicators of interest:
    # active_days_percent
    # total visits
    # median_visits_per_active_day
    # case_registered
    # follow_up_unique_case
    # median_visit_duration
    # obsnum
    # batch entry (wait on this till we have the correct data) 
    # batch entry percent (wait on this till we have the correct data) 

# do we want to exclude domains born within past 6 months from correlation report?
# subset indicators to be included in correlation report
# continuous variables
cont_vars <- c("visits", "active_days_percent", "obsnum", "median_visits_per_active_day", 
               "median_visit_duration", "case_registered", "follow_up_unique_case") 

all_cont <- all_monthly[,cont_vars]


# Correlation matrix for continuous variables 
  # source code: http://ohiodata.blogspot.com/2012/06/correlation-tables-in-r-flagged-with.html
corstarsl <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}
Rnew <- corstarsl(all_cont)
print(Rnew)
write.csv(Rnew, "corr_matrix.csv")












