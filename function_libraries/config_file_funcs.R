# FUNCTION get_run_config
# reads the file config_run.json at the specified path
# loads the json and returns the data_platform section
#
# PARAMS 
# config_path : path to config file config_run.json
get_run_config <- function (config_path) {
  config_file = file.path(config_path,"config_run.json", fsep = .Platform$file.sep)
  library("jsonlite")
  conf<-fromJSON(config_file)$data_platform
  return(conf)
}

# FUNCTION get_system_config
# reads the file config_system.json at the specified path
# loads the json and returns the data_platform section
#
# PARAMS 
# config_path : path to config file config_run.json
get_system_config <- function (config_path) {
  config_file = file.path(config_path,"config_system.json", fsep = .Platform$file.sep)
  library("jsonlite")
  conf<-fromJSON(config_file)$data_platform
  return(conf)
}

# FUNCTION get_file_paths
# gets the file paths defined in the directories section of config_system.json.
# returns 
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
# report_name : name of report module (should also be file name without .R)
get_report_options <- function (conf, report_name) {
  report_modules <- conf$reports$modules$name
  stopifnot (report_name %in% report_modules)
  
  report_section <- conf$reports
  general_options <- report_section
  general_options["modules"] <- NULL
  module_options <- report_section$modules[conf$reports$modules$name==report_name,]
  
  return(c(general_options,module_options))
}


# FUNCTION get_domain_names_include
# gets any domains to include specified by name from the run config.
# returns a list of domain names
#
# PARAMS 
# conf : the run conf json object
get_domain_names_include <- function (conf) {
  domain_names <- strsplit(as.character(conf$domains$names_include),",")[[1]]
  return(domain_names)
}

# FUNCTION get_domain_names_exclude
# gets any domains to exclude specified by name from the run config.
# returns a list of domain names
#
# PARAMS 
# conf : the run conf json object
get_domain_names_exclude <- function (conf) {
  domain_names <- strsplit(as.character(conf$domains$names_exclude),",")[[1]]
  return(domain_names)
}

single_vec_split <- function(s, split=","){
  return(strsplit(s, split)[[1]])
}

# FUNCTION get_domain_filters
# gets filterby fields and values from run config
# returns a data frame with two columns: filterby and filtervals, (list of) values to include
#
# PARAMS 
# conf : the run conf json object
get_domain_filters <- function (conf) {
  domain_filters <- conf$domains$filters
  domain_filters$values <- lapply(conf$domains$filters$values,single_vec_split,split=",")
  return(domain_filters)
}

get_domains_for_filter <- function (domain_table, filter_by, vals) {
  if (!(filter_by %in% names(domain_table))) {
    stop(sprintf ("Domain table has no attribute named %s", filter_by))
  }
  matching_rows <- domain_table[domain_table[[filter_by]] %in% vals[[1]],]
  if (nrow(matching_rows) == 0 ) {
    warning(sprintf ("No rows with values in (%s) for attribute %s", paste(vals, collapse=","),filter_by))
  }
  return (matching_rows$name)
}

get_domains_for_run <- function (domain_table,conf) {
  names_include <- get_domain_names_include(conf)
  names_exclude <- get_domain_names_exclude(conf)
  filters <- get_domain_filters(conf)
  filters_include <- filters[filters[["type"]]=="include",]
  filters_exclude <- filters[filters[["type"]]=="exclude",]
  
  domains_include <- vector()
  # get domains to include from filters
  if (length(filters_include) > 0) {
    domains_include <- domain_table$name
    for (i in 1:nrow(filters_include)) {
      inc <- filters_include[i,]
      matching_domains <- get_domains_for_filter(domain_table,filter_by=inc$filter_by,vals=inc$values)
      domains_include <- intersect(domains_include,matching_domains)
    }
  }
  
  domains_exclude <- vector()
  # get domains to exclude from filters
  if (length(filters_exclude) > 0) {
    for (j in 1:nrow(filters_exclude)) {
      exc <- filters_exclude[j,]
      matching_domains <- get_domains_for_filter(domain_table,filter_by=exc$filter_by,vals=exc$values)
      domains_exclude <- union(domains_exclude,matching_domains)
    }
  }
  
  domains_for_run <- setdiff(union(domains_include, names_include), union(domains_exclude, names_exclude))
  return (domains_for_run)
}