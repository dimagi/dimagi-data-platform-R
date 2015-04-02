# Utility functions for report modules

# FUNCTION get_aggregate_table
# gets an aggregate table by name, then filters to match only domains in domain_names
#
# PARAMS 
# db: the database to use (connection through dplyr src_postgres)
# table_name: the name of the aggregate table to fetch
# domain_names: a list of domain names to filter the aggregate table by
get_aggregate_table <- function (db,table_name, domain_names) {
  agg_table <- tbl(db, table_name)
  agg_table <- filter(agg_table, domain %in% domain_names)
  return(collect(agg_table))
}

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
add_splitby_col <- function (data_table, domain_table, splitby_var, split_name="split_by") {
  if (!(splitby_var %in% names(domain_table))) {
    stop(sprintf ("Domain table has no attribute named %s", splitby_var))
  }
  splityby_frame <- subset(domain_table, select=c("name",splitby_var))
  if (split_name == "split_by") {
    splitby_var <- colnames(splityby_frame)[2]
  }
  df <- merge(data_table,splityby_frame,by.x="domain",by.y="name")
  names(df)[names(df) == splitby_var] <- split_name
  if (typeof(df[[split_name]]) == "list") {
    df[[split_name]] <- sapply(df[[split_name]], as.factor) 
  }
  return(df)
}

add_col <- function(data_table, subset_table, colvar, colname, dt_join, ss_join) {
  if (!(colvar %in% names(subset_table))) {
    stop(sprintf ("Subset table has no attribute named %s", colvar))
  }
  if (missing(dt_join)) {
    dt_join <- "domain"
  }
  if (missing(ss_join)) {
    ss_join <- "name"
  }

  subset_frame <- subset(subset_table, select=c(ss_join, colvar))
  df <- merge(data_table, subset_frame, by.x=dt_join, by.y=ss_join, all.x=TRUE)
  print(colnames(subset_frame))
  if (colvar == ss_join) {
    colvar <- colnames(subset_frame)[2]
  }
  names(df)[names(df) == colvar] <- colname
  if (typeof(df[[colname]]) == "list") {
    df[[colname]] <- sapply(df[[colname]], as.factor) 
  }
  return(df)
}


# FUNCTION corstarsl
# creates correlation of continuous variables
# returns a triangle correlation table with significance levels. p<0.001, ***; p<0.01, **; p<0.05, *
# correlation coefficients are truncated to two decimal
#
# PARAMS
# names of continuous variables in any aggregate table
# source code: http://ohiodata.blogspot.com/2012/06/correlation-tables-in-r-flagged-with.html
corstarsl <- function(x){ 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))   ## build a new matrix that includes the correlations with their apropriate stars 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  Rnew <- as.matrix(Rnew)      ## remove upper triangle
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  Rnew <- cbind(Rnew[1:length(Rnew)-1])    ## remove last column and return the matrix (which is now a data frame)
  return(Rnew) 
}

# FUNCTION replace.df
# updates one data frame from another
# usage: replace.df(original, replacement,by=c('Name','Id'))
#
# PARAMS 
# original : the data frame to be updated
# replacement: replace matching values in original with values from this data frame
# by: vector of column names to match on
# http://stackoverflow.com/a/7972038/3852950
replace.df <- function(x,y,by,cols=NULL){
  nx <- nrow(x)
  ny <- nrow(y)
  
  bx <- x[,by,drop=FALSE]
  by <- y[,by,drop=FALSE]
  bz <- do.call("paste", c(rbind(bx, by), sep = "\r"))
  
  bx <- bz[seq_len(nx)]
  by <- bz[nx + seq_len(ny)]
  
  idx <- match(by,bx)
  idy <- match(bx,by)
  idy <- idy[!is.na(idy)]
  
  if(is.null(cols)) {
    cols <- intersect(names(x),names(y))
    cols <- cols[!cols %in% by]
  }
  
  x[idx,cols] <- y[idy,cols]
  x
}

add_country_final <- function(domain_table) {
  #Consolidate country information
  is.na(domain_table$deployment.country) <- domain_table$deployment.country == ""
  is.na(domain_table$country) <- domain_table$country == ""
  domain_table$country_final <- domain_table$deployment.country
  keep_country <- which(is.na(domain_table$deployment.country) & !is.na(domain_table$country))
  domain_table$country_final[keep_country] <- domain_table$country[keep_country]
  return(domain_table)
}

add_nbu_col <- function(domain_table, country_var_name='deployment.country') {
  inc_list <- c("California", "Canada", "United Kingdom", "United States", "United States of America", "Wales", "France", "france", "Spain", "US", "USA")
  dsi_list <- c("Afghanistan", "Bangladesh", "Burma", "India", "Indonesia", "Laos", "Myanmar", "Nepal", "Pakistan", "Philippines", "Philippines", "Thailand", "bangladesh", "india")
  dsa_list <- c("Angola", "Burundi", "Ethiopia", "Kenya", "Lesotho", "Madagascar", "Malawi", "Rwanda", "South Africa", "South Sudan", "Tanzania", "Uganda", "Zambia", "Zimbabwe", "ethiopia", "kenya", "malawi", "south africa", "south africa ", "Sri Lanka")
  dwa_list <- c("Benin", "Burkina Faso", "Ghana", "Guinea", "Mali", "Niger and Burkina Faso", "Nigeria", "Senegal", "Sierra Leone", "Togo", "senegal")
  dlac_list <- c("Brazil", "Colombia", "Dominican Republic", "Grenada", "Guatemala", "Haiti", "Mexico", "Nicaragu", "Nicaragua") 
  dmoz_list <- c("Mozambique")
  list_of_lists <- list(inc_list, dsi_list, dsa_list, dwa_list, dlac_list, dmoz_list)
  names(list_of_lists) <- c("Inc", "DSI", "DSA", "DWA", "DLAC", "DMOZ")

  set_unit <- function(business_unit, country) {
    if (is.na(business_unit) | business_unit %in% c("None", "")) {
      for(unit in names(list_of_lists)) {
        if (country %in% list_of_lists[[unit]]) {
          return(unit)
        }
      }
    }
    return(business_unit)
  }
  
  domain_table[["new_business_unit"]] <- mapply(set_unit, domain_table$business_unit, domain_table[[country_var_name]])
  return(domain_table)
}

get_post_processed_domain_table <- function(db) {
  domain_table <- get_domain_table(db)
  domain_table <- add_country_final(domain_table)
  domain_table <- add_nbu_col(domain_table, "country_final")
  return(domain_table)
}
