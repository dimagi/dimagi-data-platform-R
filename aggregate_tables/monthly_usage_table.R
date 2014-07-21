# call functions multiple times instead of working off a list of data frames
# install/import libraries 
library(zoo)
library(digest)
library(plyr)
library(timeDate)
library(reshape2)

create_monthly_tables <- function (domain_table, interaction_table, output_dir) {
  
  v <- interaction_table
  domain_names <- domain_table$name
  mainDir <- output_dir
  
  for(i in 1:length(domain_names)){
    
    source(file.path("aggregate_tables","get_interaction_section.R", fsep = .Platform$file.sep))
    data <- get_interaction_subset(test, domain_names[i])
    
    source(file.path("aggregate_tables","visit_table_run.R", fsep = .Platform$file.sep))
    source(file.path("aggregate_tables","export_tables.R", fsep = .Platform$file.sep))
    source(file.path("aggregate_tables","lifetime_run.R",fsep = .Platform$file.sep))
    source(fil.path("aggregate_tables","monthly_run.R",fsep = .Platform$file.sep))
    
    # export data
    dir.create(file.path(mainDir, domain_names[i]))
    setwd(file.path(output_dir, subDir0,fsep = .Platform$file.sep))
    tableOut(data, 1)
    tableOut(y, 2)
    tableOut(user_10, 3)
  }
}


create_tables_debug <- function (test_data_dir, output_dir){
  source(file.path("function_libraries","csv_sources.R", fsep = .Platform$file.sep),chdir=T)
  domain_table <- get_domain_table_from_csv(test_data_dir)
  v<-get_interaction_table_from_csv(test_data_dir)
}

create_tables <- function (con, output_dir){
  source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep),chdir=T)
  domain_table <- get_domain_table(con)
  v <- get_interaction_table(con, domain_table$name)
  close_con(con)
}




