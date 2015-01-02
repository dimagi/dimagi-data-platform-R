# Comparision Report!
library(zoo) #work with mm/yy calendar dates without day
library("reshape2")
library('gridExtra')
library('ggplot2')
library('xtable')


render <- function(db, domains_for_run, report_options, tmp_report_pdf_dir) {
  split_by <- report_options$split_by
  print(paste(c("split on: ", split_by)))
  source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep))
  domain_table <- get_domain_table(db)
  
  source(file.path("data_sources.R", fsep = .Platform$file.sep))
  sf_table <- get_salesforce_contract_table(db)
  
  source(file.path("function_libraries","report_utils.R", fsep = .Platform$file.sep))
  monthly_table <- get_aggregate_table (db, "aggregate_monthly_interactions", domains_for_run)
  
  #rename columns names as needed for attrition logic
  names (monthly_table)[names(monthly_table) == "month.index"] = "calendar_month"
  names (monthly_table)[names(monthly_table) == "numeric_index"] = "obsnum"
  
  active_monthly <- monthly_table[monthly_table$nforms>0 & !is.na(monthly_table$nforms), ]
  active_monthly <- add_splitby_col(active_monthly, domain_table, split_by)
  
  #Remove demo users
  #We also need to find a way to exclude admin/unknown users
  active_monthly = active_monthly[!(all_monthly$user_id =="demo_user"),]
  
  #Remove any dates before report start_date and after report end_date
  names (active_monthly)[names(active_monthly) == "first_visit_date.x"] = "first_visit_date"
  active_monthly$date_first_visit = as.Date(active_monthly$date_first_visit, origin="1970-01-01")
  active_monthly$date_last_visit = as.Date(active_monthly$date_last_visit, origin="1970-01-01")
  start_date = as.Date(report_options$start_date)
  end_date = as.Date(report_options$end_date)
  active_monthly = subset(active_monthly, active_monthly$date_first_visit >= start_date
                          & active_monthly$date_last_visit <= end_date)
  
  #add countries
  active_monthly <- add_splitby_col(active_monthly, domain_table, "deployment.country", "country")
  print(head(active_monthly))
  #add self starters
  active_monthly <- add_splitby_col(active_monthly, domain_table, "internal.self_started", "self_started")
  
  cur_month = as.yearmon(Sys.Date())
  cur_month <- "May 2013"
  recent_monthly <- active_monthly[active_monthly$calendar_month==cur_month & !is.na(active_monthly$calendar_month), ]  
  
  splits = unique(active_monthly[, "split_by"])
  splits = unique(names(sort(table(active_monthly[, 'split_by']), decreasing=TRUE)[c(1:report_options$max_groups)]))
  splits = as.vector(na.omit(replace(splits, splits %in% c("", "None"), NA))) # remove None and NA
  all_splits <- c(splits, c("None", "Total"))
  splits_with_none <- c(splits, "None")
  
  #active users
  print("active users")
  udf <- data.frame(split=all_splits, active_users=NA, attrition=NA, perc_android=NA, 
                    freq_country=NA, self_starter_users=NA, median_cases_touched=NA, 
                    median_days_active=NA, median_visit_duration=NA)
  
  ddf <- data.frame(split=all_splits, active_domains=NA, perc_android=NA, freq_country=NA, 
                    self_starter_domains=NA, services_income=NA, all_time_income=NA)
  split_out <- function(frame, split, splits) {
    if (split %in% splits) {
      return(frame[frame$split_by==split & !is.na(frame$split_by) ,])
    } else if (split == 'None') {
      return(frame[frame$split_by=="None" | is.na(frame$split_by), ])
    } else {
      return(frame)
    }
  }
  
  for (split in all_splits) {
    print(split)
    split_monthly <- split_out(recent_monthly, split, splits)
    num_active_users <- nrow(split_monthly)
    num_active_domains <- length(unique(split_monthly[["domain"]]))
    print(num_active_users)
    udf[udf$split==split, 2] <- num_active_users
    ddf[ddf$split==split, 2] <- num_active_domains
  }
  
  # % users with android
  print("users with android")
  for (split in all_splits) {
    split_monthly <- split_out(recent_monthly, split, splits)
    num_active_users <- nrow(split_monthly)
    android_monthly = split_monthly[split_monthly$summary_device_type=="Android",]
    perc_android_users = (nrow(android_monthly) / num_active_users) * 100
    
    num_active_domains <- length(unique(split_monthly[["domain"]]))
    num_android_domains <- length(unique(android_monthly[["domain"]]))
    perc_android_domains = (num_android_domains / num_active_domains) * 100
    
    udf[udf$split==split, 4] <- round(perc_android_users, digits=2)
    ddf[ddf$split==split, 3] <- round(perc_android_domains, digits=2)
  }
  
  #Perform the following function by flw
  #If the flw has been around for only one month
  #That month will always have retained = F and addition = T
  #If the flw has been around for more than one month
  #The last month for each flw will always have retained = F
  #The first month for each flw will always have addition = T 
  #The next t row should always be just one step up from the previous t row (retained = T)
  #If not equal, then the FLW was not retained (retained = F) 
  #The next t row should always be just one step up from the previous t row (addition = F)
  #If not equal, then the flw was added (addition = T) 
  
  retain_add <- function(x) {
    if (length(x$obsnum) == 1) {
      x$retained <- FALSE
      x$addition <- TRUE
    }
    else {
      x$retained <- c(x$obsnum[1:(length(x$obsnum)-1)] + 1 == x$obsnum[2:length(x$obsnum)], FALSE)
      x$addition <- c(TRUE, x$obsnum[2:length(x$obsnum)] != x$obsnum[1:(length(x$obsnum)-1)] + 1)
    }
    return(x)
  }
  
  #Get rid of calendar_month for this part because it is not supported for this
  #operation. We can always add it back later.
  df1 = select(active_monthly, -calendar_month)
  df1 = arrange(df1, user_id, obsnum)
  df_group = group_by(df1, user_id)
  df2 <- retain_add(df_group)
  
  #This gives the attrition rate in the next month, using the # in obsnum as the denominator
  #Numerator is # of flws that are not retained in the next month from the previous month
  #This also gives the addition rate in the obsnum month, using # in obsnum as the denominator
  #Numerator is # of flws that were added in for that obsnum  
  attrition_table = ddply(df2, .(obsnum), 
                          function(x) c(attrition=mean(!x$retained)*100, 
                                        additions=mean(x$addition)*100))
  
  attrition_table_split = ddply(df2, .(obsnum, split_by), 
                                function(x) c(attrition=mean(!x$retained)*100, 
                                              additions=mean(x$addition)*100))
  
  for (split in all_splits) {
    # att_rows = attrition_table_split[attrition_table_split$split_by==split,]
    att_rows <- split_out(attrition_table_split, split, splits)
    udf[udf$split==split, 3] <- round(mean(att_rows$attrition, na.rm = TRUE), digits=2)
  }
  
  # function for getting most frequent occurence of vector
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # most frequent country  
  print("most frequent country")
  for (split in all_splits) {
    print(split)
    split_monthly <- split_out(recent_monthly, split, splits)
    countries_users <- split_monthly[["country"]]
    freq_country_name_users <- Mode(na.omit(countries_users[countries_users!=""]))
    domains <- unique(split_monthly[["domain"]])
    if (length(domains) == 0) {
      udf[udf$split==split, 5] <- "No country set (100%)"
      ddf[ddf$split==split, 4] <- "No country set (100%)"
      next
    }
    countries_domains <- c()
    for (domain in domains) {
      country_name <- split_monthly[split_monthly$domain==domain, ][["country"]][1]
      countries_domains <- c(countries_domains, country_name)
    }
    freq_country_name_domains <- Mode(na.omit(countries_domains[countries_domains!=""]))
    
    print(freq_country_name_users)
    if (is.na(freq_country_name_users)) {
      udf[udf$split==split, 5] <- "No country set (100%)"
    } else {
      num_country_users <- nrow(split_monthly[split_monthly$country==freq_country_name_users, ])
      perc_country <- round((num_country_users / nrow(split_monthly)) * 100, digits=0)
      udf[udf$split==split, 5] = paste(c(freq_country_name_users, " (", perc_country, "%) "), collapse="")
    }
    print(freq_country_name_domains)
    if (is.null(freq_country_name_domains) | is.na(freq_country_name_domains)) {
      ddf[ddf$split==split, 4] <- "No country set (100%)"
    } else {
      freq_monthly <- split_monthly[split_monthly$country==freq_country_name_domains, ]
      perc_country <- round((length(unique(freq_monthly[["domain"]])) / length(domains)) * 100, digits=0)
      ddf[ddf$split==split, 4] = paste(c(freq_country_name_domains, " (", perc_country, "%) "), collapse="")
    }
  }
  
  
  # percent of self starters
  print("self starters")
  for (split in all_splits) {
    print(split)
    split_monthly <- split_out(recent_monthly, split, splits)
    self_started <- split_monthly[split_monthly$self_started=="True", ]
    num_domains <- length(unique(self_started[["domain"]]))
    num_users <- nrow(self_started)
    perc_self_started_domains <- round((num_domains / ddf[ddf$split==split, 2]) * 100, digits=0)
    perc_self_started_users <- round((num_users / udf[udf$split==split, 2]) * 100, digits=0)
    udf[udf$split==split, 6] <- paste(c(num_users, " (", perc_self_started_users, "%)"), collapse="")
    ddf[ddf$split==split, 5] <- paste(c(num_domains, " (", perc_self_started_domains, "%)"), collapse="")
  }
  
  # income
  print("calculating income")
  domains <- data.frame(domain=unique(active_monthly$domain))
  saas_info <- add_col(domains, sf_table, "services_income__c", "services", "domain", "domain")
  saas_info <- add_col(saas_info, sf_table, "all_time_income__c", "alltime", "domain", "domain")
  
  cash_bracket <- function(amt) {
    if (amt < 1) { return("zero") }
    if (amt < 100) { return("one") }
    if (amt < 500) { return("hundred") }
    if (amt < 10000) { return("five_hundred") }
    if (amt < 30000) { return("ten_k") }
    if (amt < 100000) { return("thirty_k") }
    if (amt < 10000000) { return("hundred_k") }
  }
  
  top_domains_df <- data.frame(split=all_splits, alltime=NA, services=NA)
  services_grouping <- data.frame(split=splits_with_none, zero=0, one=0, hundred=0, 
                                  five_hundred=0, ten_k=0, thirty_k=0, hundred_k=0)
  alltime_grouping <- data.frame(split=splits_with_none, zero=0, one=0, hundred=0, 
                                 five_hundred=0, ten_k=0, thirty_k=0, hundred_k=0)
  
  for (split in all_splits) {
    split_monthly <- split_out(recent_monthly, split, splits)
    domains <- unique(split_monthly[["domain"]])
    
    alltime_info <- saas_info[saas_info$domain %in% domains & !is.na(saas_info$alltime), ][c("domain", "alltime")]
    top_alltime_domains <- head(alltime_info[order(-alltime_info$alltime), ], 5)
    top_alltime_domains$alltime <- sapply(top_alltime_domains$alltime, round)
    alltime_str_bits <- apply(top_alltime_domains, 1, function(x) paste(x, collapse= ": $"))
    top_domains_df[top_domains_df$split==split, 'alltime'] <- paste(alltime_str_bits, collapse=" | ")
    alltime_mapping <- as.data.frame(table(sapply(alltime_info$alltime, cash_bracket)))
    for (var in alltime_mapping$Var1) {
      alltime_grouping[alltime_grouping$split==split, var] <- alltime_mapping[alltime_mapping$Var1==var, "Freq"]
    } 
    
    services_info <- saas_info[saas_info$domain %in% domains & !is.na(saas_info$services), ][c("domain", "services")]
    top_services_domains <- head(services_info[order(-services_info$services), ], 5)
    top_services_domains$services <- sapply(top_services_domains$services, function(x) as.character(round(x)))
    services_str_bits <- apply(top_services_domains, 1, function(x) paste(x, collapse= ": $"))
    top_domains_df[top_domains_df$split==split, 'services'] <- paste(services_str_bits, collapse=" | ")
    services_mapping <- as.data.frame(table(sapply(services_info$services, cash_bracket)))
    for (var in services_mapping$Var1) {
      services_grouping[services_grouping$split==split, var] <- services_mapping[services_mapping$Var1==var, "Freq"]
    } 
        
    services_income <- sum(saas_info[saas_info$domain %in% domains & !is.na(saas_info$services), ][["services"]])
    alltime_income <- sum(saas_info[saas_info$domain %in% domains & !is.na(saas_info$alltime), ][["alltime"]])
    ddf[ddf$split==split, 6] = round(services_income)
    ddf[ddf$split==split, 7] = round(alltime_income)
  }
  
  months <- sort(as.yearmon(unique(active_monthly[["calendar_month"]])))
  create_month_plot_table <- function() {
    num_cols <- 1 + length(months)
    num_rows <- length(splits_with_none)
    table <- as.data.frame(matrix(0, ncol=num_cols, nrow=num_rows))
    names(table) <- c("split", as.character(months))
    table[["split"]] <- splits_with_none
    return(table)
  }
  
  # active users overview table
  active_users_table <- create_month_plot_table()
  active_domains_table <- create_month_plot_table()
  android_users_table <- create_month_plot_table()
  android_domains_table <- create_month_plot_table()
  self_start_users_table <- create_month_plot_table()
  self_start_domains_table <- create_month_plot_table()
  cases_touched_table <- create_month_plot_table()
  active_days_table <- create_month_plot_table()
  visit_duration_table <- create_month_plot_table()
  
  for (month in as.character(months)) {
    month_data <- active_monthly[active_monthly$calendar_month==month, ]
    for (split in splits_with_none) {
      split_data <- split_out(month_data, split, splits)
      num_users <- nrow(split_data)
      num_domains <- length(unique(split_data[["domain"]]))

      android = split_data[split_data$summary_device_type=="Android",]
      perc_android_users <- round((nrow(android) / num_users) * 100, digits=2)
      perc_android_domains <- round((length(unique(android[["domain"]])) / num_domains) * 100, digits=2)
      
      self_started <- split_data[split_data$self_started=="True", ]
      perc_ss_users <- round((nrow(self_started) / num_users) * 100, digits=2)
      perc_ss_domains <- round((length(unique(self_started[["domain"]])) / num_domains) * 100, digits=2)
      
      median_cases_touched <- median(na.omit(split_data[['ncases_touched']]))
      median_active_days <- median(na.omit(split_data[['active_days']]))
      median_visit_duration <- median(na.omit(split_data[['median_visit_duration']]))
      
      if (cur_month == month) {
        udf[udf$split==split, 7] <- median_cases_touched
        udf[udf$split==split, 8] <- median_active_days
        udf[udf$split==split, 9] <- median_visit_duration
      }
      
      active_users_table[active_users_table$split==split, ][[month]] <- num_users
      active_domains_table[active_domains_table$split==split, ][[month]] <- num_domains
      android_users_table[android_users_table$split==split, ][[month]] <- perc_android_users
      android_domains_table[android_domains_table$split==split, ][[month]] <- perc_android_domains
      self_start_users_table[self_start_users_table$split==split, ][[month]] <- perc_ss_users
      self_start_domains_table[self_start_domains_table$split==split, ][[month]] <- perc_ss_domains
      active_days_table[active_days_table$split==split, ][[month]] <- median_active_days
      cases_touched_table[cases_touched_table$split==split, ][[month]] <- median_cases_touched
      visit_duration_table[visit_duration_table$split==split, ][[month]] <- median_visit_duration
    }
  }
  udf[udf$split=="Total", 7] <- median(na.omit(recent_monthly[['ncases_touched']]))
  udf[udf$split=="Total", 8] <- median(na.omit(recent_monthly[['active_days']]))
  udf[udf$split=="Total", 9] <- median(na.omit(recent_monthly[['median_visit_duration']]))
  
  #-----------------------------------------------------------------------------#
  #PRINT PLOTS AND EXPORT TO PDF
  #-----------------------------------------------------------------------------#
  module_pdfs <- list()
  report_output_dir <- file.path(tmp_report_pdf_dir, "reports")
  dir.create(report_output_dir, showWarnings = FALSE)
  
  custom_plot <- function(a_table, value_name, variable_name) {
    #detailed for active users
    colnames(a_table)[1] <- split_by
    ch <- melt(a_table, id.vars=split_by, value.name=value_name, variable.name=variable_name)
    ggplot(data=ch, aes_string(x = variable_name, y = value_name, group = split_by, colour = split_by)) +
      geom_line() +
      geom_point( size=2, shape=21, fill="white") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
  
  plot_grouping <- function(df) {
    new_col_names <- c(split_by, 'x <= 0', '0 < x < 100', '100 < x < 500', '500 < x < 10000', 
                       '10000 < x < 30000', '30000 < x < 100000', 'x > 100000')
    colnames(df) <- new_col_names
    dfm <- melt(df[, new_col_names], id.vars = 1)
    value <- "domains"
    variable <- "cash_bracket"
    colnames(dfm) <- c(split_by, variable, value)
    ggplot(dfm, aes_string(x = split_by, y = value)) + 
      geom_bar(aes_string(fill = variable), position = "dodge", stat = "identity")
  }
  
  names(udf) <- c(split_by, "Active Users", "Attrition", "% Android", "Most Common Country", 
                  "Self Starters", "Median Cases Touched", "Median Active Days", "Median Visit Duration")
  names(ddf) <- c(split_by, "Active Domains", "% Android", "Most Common Country", 
                  "Self Starters", "Services Income", "All Time Income")
  names(top_domains_df) <- c(split_by, "Top 5 All-Time Income", "Top 5 Services Income")
  rmarkdown::render('report_templates/comparison_report.Rmd')
  
  
#   
#   outfile <- file.path(report_output_dir,"Comparison_Report.pdf")
#   print(outfile)
#   pdf(outfile, height=8.5, width=16)
#   
#   dev.off()
#   module_pdfs <- c(module_pdfs, outfile)
#   print(module_pdfs)
#   
  return(module_pdfs)
}