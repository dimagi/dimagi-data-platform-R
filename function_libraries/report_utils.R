# Utility functions for report modules

# FUNCTION merged_monthly_table
# gets all monthly.csv files for domains specified in domain_names
# returns one merged table with all columns present in any monthly.csv
#
# PARAMS 
# domain_names: a list of domain names to import monthly.csv files for
# aggregate_tables_dir: the directory for aggregate tables,  should be output_dir/aggregate_tables
merged_monthly_table <- function (domain_names, aggregate_tables_dir) {
  library(plyr)
  
  read_csv_add_domain <- function (dname) {
    monthly_table_file <- file.path (aggregate_tables_dir,dname,"monthly.csv", fsep = .Platform$file.sep)
    df <- read.csv(monthly_table_file,header = TRUE)
    df$domain <- dname
    return(df)
  }
  
  merged <- rbind.fill(lapply(domain_names, read_csv_add_domain))
  return(merged)
}

# FUNCTION add_splitby_col
# pulls the values of splitby_var from the domain table
# adds then as a factor a column named split-by to the data table and returns it
#
# PARAMS 
# data_table : the data to add a split_by column to
# domain_table: the domain table
# splitby_var: the variable to split by
add_splitby_col <- function (data_table, domain_table, splitby_var) {
  if (!(splitby_var %in% names(domain_table))) {
    stop(sprintf ("Domain table has no attribute named %s", splitby_var))
  }
  splityby_frame <- subset(domain_table, select=c("name",splitby_var))
  df <- merge(data_table,splityby_frame,by.x="domain",by.y="name")
  names(df)[names(df) == splitby_var] <- 'split_by'
  df$split_by <- sapply(df$split_by, as.factor)
  return(df)
}


# FUNCTION add_numeric_index
# adds the numeric_index column to the monthly merged table
#
# PARAMS 
# all_monthly : the monthly merged table - all users for all domains for all months
add_numeric_index <- function(all_monthly) {
  source(file.path("aggregate_tables","monthly_func.R", fsep = .Platform$file.sep))
  all_monthly$month.index = as.yearmon(all_monthly$month.index, "%b-%y")
  all_monthly <- all_monthly[order(all_monthly$domain, 
                                   all_monthly$user_id, all_monthly$month.index),]
  all_monthly$months_since_origin <- sapply(all_monthly$first_visit_date,monnb)
  domain_frames <- split(all_monthly, as.factor(all_monthly$domain))
  
  get_numeric_index <- function(x) {
    x <- ddply(x, .(user_id), transform, 
               diff = c(0,lag(months_since_origin)))
    x <- ddply(x, .( user_id), transform, 
               numeric_index = cumsum(diff) + 1)
  }
  domain_frames <- lapply(domain_frames, get_numeric_index)
  all_monthly <- unsplit(domain_frames, f = as.factor(all_monthly$domain))
  return(all_monthly)
}