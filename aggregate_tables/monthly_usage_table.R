# call functions multiple times instead of working off a list of data frames
# install/import libraries 
library(zoo)
library(digest)
library(plyr)
library(timeDate)
library(reshape2)

create_monthly_tables <- function (domain_table, interaction_table, output_dir) {
  
  v <<- interaction_table
  domain_names <<- domain_table$name

  for(dname in domain_names){
    print(sprintf("creating monthly aggregate tables for domain %s ", dname))
    dat <- v[v$domain == dname, ]
    
    assign("dat", dat, envir = .GlobalEnv)
    assign("dname", dname, envir = .GlobalEnv)
    
    if (nrow(dat) > 0) {
      source(file.path("aggregate_tables","visit_table_run.R", fsep = .Platform$file.sep))
      source(file.path("aggregate_tables","lifetime_run.R",fsep = .Platform$file.sep))
      source(file.path("aggregate_tables","monthly_run.R",fsep = .Platform$file.sep))
      
      # export data
      source(file.path("aggregate_tables","export_tables.R", fsep = .Platform$file.sep))
      
      d_path <- file.path(output_dir, dname,fsep = .Platform$file.sep)
      dir.create(d_path,  showWarnings = FALSE)
      tableOut(d_path,dat, 1)
      tableOut(d_path,y, 2)
      tableOut(d_path,user_10, 3)
    }
  }
}


create_tables_debug <- function (test_data_dir, output_dir){
  source(file.path("function_libraries","csv_sources.R", fsep = .Platform$file.sep),chdir=T)
  domain_table <- get_domain_table_from_csv(test_data_dir)
  v<-get_interaction_table_from_csv(test_data_dir)
  create_monthly_tables(domain_table,v, output_dir)
}

create_tables <- function (con, output_dir){
  source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep),chdir=T)
  domain_table <- get_domain_table(con)
  domain_table <-domain_table[domain_table$name,]
  v <- get_interaction_table(con, domain_table$name)
  create_monthly_tables(domain_table,v, output_dir)
  close_con(con)
}




