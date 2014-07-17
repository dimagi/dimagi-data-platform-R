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

# FUNCTION get_report_module_names
# gets the names of the report modules from the run config.
# returns a list of report module names
#
# PARAMS 
# config_path : path to config file config_run.json
get_report_module_names <- function (config_path) {
  conf <- get_run_config(config_path)
  report_modules <- conf$reports$modules$name
  return(report_modules)
}

# FUNCTION get_report_options
# gets all report configuration options for the named report module -
# both module-specific and general report run config
# returns a list of all config options that apply to a specific report.
#
# PARAMS 
# config_path : path to config file config_run.json
# report_name : name of report module (should also be file name without .R)
get_report_options <- function (config_path, report_name) {
  conf <- get_run_config(config_path)
  report_modules <- conf$reports$modules$name
  stopifnot (report_name %in% report_modules)
  
  report_section <- conf$reports
  general_options <- report_section
  general_options["modules"] <- NULL
  module_options <- report_section$modules[conf$reports$modules$name==report_name,]
  
  return(c(general_options,module_options))
}


# FUNCTION get_domain_names
# gets any domains specified by name from the run config.
# returns a list of domain names
#
# PARAMS 
# config_path : path to config file config_run.json
get_domain_names <- function (config_path) {
  conf <- get_run_config(config_path)
  domain_names <- conf$domains$names$name
  return(domain_names)
}

# FUNCTION get_domain_filters
# gets filterby fields and values from run config
# returns a data frame with two columns: filterby and filtervals, (list of) values to include
#
# PARAMS 
# config_path : path to config file config_run.json
get_domain_filters <- function (config_path) {
  conf <- get_run_config(config_path)
  filterbys <- conf$domains$filters$filterby
  filtervals <- conf$domains$filters$values
  domain_filters <- as.data.frame(cbind (filterbys,filtervals))
  domain_filters<-transform(domain_filters, filtervals = strsplit(as.character(filtervals),split=","))
  return(domain_filters)
}