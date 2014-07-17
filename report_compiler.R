# called to run an R report module through Rscript, with command line arguments.

# get command line args (passed to Rscript)
args <- commandArgs(trailingOnly = TRUE)

r_script_path <- args[1] # where to find the other scripts we're sourcing
config_path <- args[2] # where to find config.json

# get the config parameters
source (file.path(r_script_path,"data_platform_funcs.R", fsep = .Platform$file.sep))
conf <- get_config(config_path)

# should the script use a csv file for testing instead of a database query?
use_csv <- FALSE

# run the report
source (file.path(r_script_path,report_module_script, fsep = .Platform$file.sep))