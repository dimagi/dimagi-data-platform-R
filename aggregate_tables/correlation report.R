# indicators of interest:
    # active_days_per_month
    # active_days_percent
    # total forms submitted in a month
    # total time spent on forms in a month
    # batch entry 
    # batch entry percent
    # different clients visited in a month
    # case registered in a month



# import data from somewhere
# monthly_merge <- .... 

# do we want to exclude domains born within past 6 months from correlation report?

# subset indicators to be included in correlation report
cont_vars <- c("visits", "nforms_per_month", "numeric_index", "days_on_cc", "median_visits_per_active_day", "median_visit_duration", 
          "active_days_percent", "batch_entry_percent", "new_case_percent", "follow_up_percent") # continuous variables
cont_vars <- monthly_merge[, cont_vars]


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
Rnew <- corstarsl(cont_vars)
print(Rnew)
write.csv(Rnew, "corr_matrix.csv")












