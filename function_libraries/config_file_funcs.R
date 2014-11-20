# FILE config_file_funcs.R
# convenience functions to access configuration options from run_config.json and system_config.json

library("jsonlite")
library("dplyr")

# FUNCTION get_run_config
# reads the file config_run.json at the specified path
# loads the json and returns the data_platform section
#
# PARAMS 
# config_path : path to config file config_run.json
get_run_config <- function (path_to_run_config) {
  conf<-fromJSON(path_to_run_config)$data_platform
  return(conf)
}

# FUNCTION get_system_config
# reads the file config_system.json at the specified path
# loads the json and returns the data_platform section
#
# PARAMS 
# config_path : path to config file config_run.json
get_system_config <- function (path_to_system_config) {
  conf<-fromJSON(path_to_system_config)$data_platform
  return(conf)
}

# FUNCTION get_db_connection
# gets a dplyr database connection using the params in system_conf
#
# PARAMS 
# config : content of config_system.json
get_db_connection <- function(config) {
  db_config <- config[['database']]
  if ('pass' %in% names(db_config)) {
    names(db_config)[names(db_config) == 'pass'] <- 'password'
  }
  db <- do.call(src_postgres, db_config)
  return(db)
}

# FUNCTION get_file_paths
# gets the file paths defined in the directories section of config_system.json
#
# PARAMS 
# conf : the system conf json object
get_file_paths <- function (conf) {
  dirs <- conf$directories
  return (dirs)
}

# FUNCTION get_report_module_names
# gets the names of the report modules from the run config.
# returns a list of report module names
#
# PARAMS 
# conf : the run conf json object
get_report_module_names <- function (conf) {
  report_modules <- conf$reports$modules$name
  return(report_modules)
}

# FUNCTION get_aggregate_table_names
# gets the names of the aggregate tables from the system config.
# returns a list of aggregate table names
#
# PARAMS 
# conf : the system conf json object
get_aggregate_table_names <- function (conf) {
  agg_tables <- conf$aggregate_tables$name
  return(agg_tables)
}

# FUNCTION get_report_options
# gets all report configuration options for the named report module -
# both module-specific and general report run config
# returns a list of all config options that apply to a specific report.
#
# PARAMS 
# conf : the run conf json object
# report_name : name of report module (should always be file name without .R)
get_report_options <- function (conf, report_name) {
  report_modules <- conf$reports$modules$name
  stopifnot (report_name %in% report_modules)
  
  report_section <- conf$reports
  general_options <- report_section
  general_options["modules"] <- NULL
  module_options <- report_section$modules[conf$reports$modules$name==report_name,]
  
  return(c(general_options,module_options))
}


# FUNCTION get_named_domains
# gets any domains to include or exclude specified by name from the run config.
# returns a list of domain names
#
# PARAMS
# spec_type : one of "include" or "exclude"
# conf : the run conf json object
get_named_domains <- function (spec_type,conf) {
  section <- sprintf("names_%s",spec_type)
  if (section %in% names(conf$domains)) {
    domain_names <- strsplit(as.character(conf$domains[[section]]),",")[[1]]
  } else {
    domain_names <- vector()
  }
  return(domain_names)
}

# FUNCTION get_domain_filters
# gets filterby fields and values from run config
# returns a data frame with two columns: filterby and filtervals, (list of) values to include
#
# PARAMS 
# conf : the run conf json object
get_domain_filters <- function (conf) {
  if ("filters" %in% names(conf$domains)) {
    domain_filters <- conf$domains$filters
    
    if ("values" %in% names(domain_filters)) { # an include-all filter doesn't have values
      
      domain_filters$values <- conf$domains$filters$values
    }
  }
  else
  {
    domain_filters <- list()
  }
  return(domain_filters)
}

# FUNCTION get_domain_filters
# gets domains matching a single filter, specified by filter_by and vals
# returns a list of domain names
#
# PARAMS 
# domain_table : the full domain table
# filter_by : the name of the attribute (domain table column) to filter by
# vals : domains with any of these values for the filter attribute should be returned - they match the filter
get_domains_for_filter <- function (domain_table, filter_by, vals) {
  if (!(filter_by %in% names(domain_table))) {
    stop(sprintf ("Domain table has no attribute named %s", filter_by))
  }
  filter_vals <- vals[[1]]
  any_vals_match <- function(row_vals,filter_vals) return (sum(row_vals %in% filter_vals,na.rm=T)>0)
  matching_rows <- domain_table[sapply(domain_table[[filter_by]],any_vals_match,filter_vals),]
  
  if (nrow(matching_rows) == 0 ) {
    warning(sprintf ("No rows with values in (%s) for attribute %s", paste(vals, collapse=","),filter_by))
  }
  return (matching_rows$name)
}

get_domains_for_run <- function (domain_table,conf) {
  names_include <- get_named_domains("include",conf)
  names_exclude <- get_named_domains("exclude",conf)
  domains_include <- vector()
  domains_exclude <- vector()
  filters <- get_domain_filters(conf)
  
  has_filters <- length(filters) > 0 # if filters is a list, you'll get a warning
  if (has_filters) { 
    filters_all <- filters[filters[["type"]]=="include-all",]
    filters_include <- filters[filters[["type"]]=="include",]
    filters_exclude <- filters[filters[["type"]]=="exclude",]

    if (NROW(filters_all) > 0) {  # if we have an include-all, all domains are included unless excluded in a filter or by name
      domains_include <- domain_table$name
    }
    else if (NROW(filters_include) > 0) {  # else get domains to include from filters by domain attrs
      domains_include <- domain_table$name
      for (i in 1:nrow(filters_include)) {
        inc <- filters_include[i,]
        matching_domains <- get_domains_for_filter(domain_table,filter_by=inc$filter_by,vals=inc$values)
        domains_include <- intersect(domains_include,matching_domains)
      }
    }
    
    # get domains to exclude from filters
    if (NROW(filters_exclude) > 0) {
      for (j in 1:nrow(filters_exclude)) {
        exc <- filters_exclude[j,]
        matching_domains <- get_domains_for_filter(domain_table,filter_by=exc$filter_by,vals=exc$values)
        domains_exclude <- union(domains_exclude,matching_domains)
      }
    }
  }
  
  # domains included by filter are included unless they match an exclude filter or are excluded by name
  domains_for_run <- setdiff(domains_include, union(domains_exclude, names_exclude))
  # named include domains are always included
  domains_for_run <- as.vector(rbind(names_include,domains_for_run))
  
  # finally, remove domains we don't have permission use data for
  if (!('permitted_data_only' %in% names(conf)) | (conf$permitted_data_only == T)){
    permitted_domains = get_domains_for_filter(domain_table,filter_by='internal.can_use_data',vals=c('None','True',NA))
    domains_for_run = intersect(domains_for_run, permitted_domains)
  }
  
  return (domains_for_run)
}