require(dplyr)

create_tables <- function (con,aggtables_output_subdir) {
  source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep),chdir=T)
  device_type_table <- get_device_type_table(con)
  create_monthly_device_type_table(device_type_table, aggtables_output_subdir)
  close_con(con)
}

create_tables_debug <- function (test_data_dir,aggtables_output_subdir) {
  
}

get_device_type_for_month <- function (values) {
  if (length(unique(values)) == 1) {
    s <- paste(toupper(substring(values[1], 1,1)), substring(values[1], 2),
          sep="", collapse=" ")
    return (s)
  } else {
    return ('multi')
  }
}

create_monthly_device_type_table <- function (device_type_table, aggtables_output_subdir) {
  
  domain_names <- unique(device_type_table$domain_name)
  
  for(dname in domain_names) {
    
    print(sprintf("creating monthly device type table for domain %s ", dname))
    device_subset <- subset(device_type_table,domain_name == dname)
    device_subset$yrmon <-  as.factor(as.yearmon(device_subset$time_start))
    
    devices_for_month <- device_subset %.%
      group_by(domain_name,user_id,yrmon) %.%
      summarise(devices_for_month = get_device_type_for_month(device)) %.%
      arrange(user_id,yrmon)
    
    dir_path <- file.path(aggtables_output_subdir, dname, fsep = .Platform$file.sep)
    dir.create(dir_path, showWarnings = FALSE)
    file_path <- file.path(dir_path,"device_type_monthly.csv")
    write.csv(devices_for_month,file_path)
    
  }
}