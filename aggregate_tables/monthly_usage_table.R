# call functions multiple times instead of working off a list of data frames
# install/import libraries 
library(zoo)
library(digest)
library(plyr)
library(timeDate)
library(reshape2)

create_monthly_tables <- function (domain_table, interaction_table, output_dir) {
  
  v <- interaction_table
  v$user_id[is.na(v$user_id)] <- "NONE"
  domain_names <- domain_table$name
  
  for(dname in domain_names){
    print(sprintf("creating monthly aggregate tables for domain %s ", dname))
    dat <- v[v$domain == dname, ]
    
    if (nrow(dat) > 0) {
      tryCatch({
        
        source(file.path("aggregate_tables","visit_table_run.R", fsep = .Platform$file.sep))
        visit_table <- makeVisit(dat)
        
        source(file.path("aggregate_tables","lifetime_run.R",fsep = .Platform$file.sep))
        lifetime_table <- makeLifeTime(visit_table)
        
        source(file.path("aggregate_tables","monthly_run.R",fsep = .Platform$file.sep))
        monthly_table <- makeMonthly(visit_table)
        
        # export data
        source(file.path("aggregate_tables","export_tables.R", fsep = .Platform$file.sep))
        
        d_path <- file.path(output_dir, dname,fsep = .Platform$file.sep)
        dir.create(d_path,  showWarnings = FALSE)
        tableOut(d_path,visit_table, 1)
        tableOut(d_path,lifetime_table, 2)
        tableOut(d_path,monthly_table, 3)
      }, error = function(cond) {
        message("Error, could not generate monthly table")
        message(cond)
      })
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
  v <- get_interaction_table(con, domain_table$name)
  create_monthly_tables(domain_table,v, output_dir)
  close_con(con)
}




