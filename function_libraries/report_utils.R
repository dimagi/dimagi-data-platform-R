# Utility functions for report modules

# FUNCTION merged_monthly_table
# gets all monthly.csv and device_type_monthly files for domains specified in domain_names
# returns one merged table with all columns present in any monthly aggregate table csv file
#
# PARAMS 
# domain_names: a list of domain names to import monthly aggregate table csv files for
# aggregate_tables_dir: the directory for aggregate tables,  should be output_dir/aggregate_tables
merged_monthly_table <- function (domain_names, aggregate_tables_dir) {
  read_csv_add_domain <- function (dname) {
    tryCatch({
      monthly_table_file <- file.path (aggregate_tables_dir,dname,"monthly.csv", fsep = .Platform$file.sep)
      df <- read.csv(monthly_table_file,header = TRUE)
      devices_table_file <- file.path (aggregate_tables_dir,dname,"device_type_monthly.csv", fsep = .Platform$file.sep)
      devs <- read.csv(devices_table_file,header = TRUE)
      df$domain <- dname
      df <- merge(x=df,y=devs,by.x=c("user_id","month.index"),by.y=c("user_id","yrmon"))
      return(df)
    }, error = function(cond) {
      message(sprintf("\nError, could not load monthly table for domin %s.",dname))
      message(cond)
    })
  }
  all_monthly_tables <- lapply(domain_names, read_csv_add_domain)
  merged <- rbind.fill(all_monthly_tables)
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