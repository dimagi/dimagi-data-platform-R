# database queries using raw SQL
# for aggregate table data sources, use data_sources.R

library(dplyr)
library(DBI)
library(RPostgreSQL)
library(reshape2)

do_query <-function (con, query) {
  rs <- dbSendQuery(con,query)
  v <- fetch(rs,n=-1)
  dbClearResult(rs)
  return (v)
}

# returns domain table including sector and subsector names as lists
get_domain_table <- function (db) {
  con <- db$con
  normal_cols_q <- "select * from domain order by name"
  normal_cols <- do_query(con,normal_cols_q)
  retframe<-normal_cols[, !(colnames(normal_cols) %in% c("attributes"))]
  
  hstore_keyvalues_q <- "select name, (each(attributes)).* from domain order by name"
  hstore_keyvalues <- do_query(con,hstore_keyvalues_q)
  hstore_keyvalues <- hstore_keyvalues[!(hstore_keyvalues$key %in% c('name','organization')),] # remove or we have duplicate colnames
  hstore_keyvalues <- hstore_keyvalues[!grepl("^_attachments",hstore_keyvalues$key),] # attachment properties are custom by domain
  hstore_wide<-dcast(hstore_keyvalues, name ~ key)
  retframe<-merge(retframe,hstore_wide,by="name",all=T)
  
  sectors_q <- "select domain.name, 
              array_to_string(array(select sector.name 
              from sector, domain_sector 
              where domain_sector.domain_id = domain.id 
              and domain_sector.sector_id=sector.id),'\t') as sector
              from domain
              order by domain.name"
  sectors <- do_query(con,sectors_q)
  sectors<-transform(sectors, sector = strsplit(sector,split="\t"))
  sectors$sector[sapply(sectors$sector,length)==0]<-NA
  retframe<-merge(retframe,sectors,by="name",all=T)
  
  subsectors_q <- "select domain.name, 
              array_to_string(array(select subsector.name 
              from subsector, domain_subsector 
              where domain_subsector.domain_id = domain.id 
              and domain_subsector.subsector_id=subsector.id),'\t') as subsector
              from domain
              order by domain.name"
  subsectors <- do_query(con,subsectors_q)
  subsectors<-transform(subsectors, subsector = strsplit(subsector,split="\t"))
  subsectors$subsector[sapply(subsectors$subsector,length)==0]<-NA
  retframe<-merge(retframe,subsectors,by="name",all=T)
  
  retframe <- data.frame(retframe,check.names=T)
  return(retframe)
}

get_application_table <- function(db){
  q <- 'select domain.name as domain_name, application.app_id, application.app_name 
  from application, domain where application.domain_id = domain.id'
  res <- collect(tbl(db,sql(q)))
  
  attrs_q <- "select app_id, (each(attributes)).* from application"
  attrs <- collect(tbl(db,sql(attrs_q)))
  attrs <- attrs[!(attrs$key %in% c('domain','app_id','app_name')),] # remove or we have duplicate colnames
  attrs <- attrs[!grepl("^_attachments",attrs$key),] # attachment properties are custom
  attrs <- attrs[!grepl("^multimedia_map",attrs$key),] # multimedia map items
  attrs_wide<-dcast(unique(attrs), app_id ~ key, value.var="value")
  res<-merge(res,attrs_wide,by="app_id",all=T)
  
  return (res)
}

get_salesforce_contract_data <- function (db) {
  q <- "select domain.name as dname, sf_dcontract__c.*
          from domain, sf_dcontract__c
          where attributes ? 'internal.sf_contract_id' 
          and attributes->'internal.sf_contract_id'  <> ''
          and attributes->'internal.sf_contract_id'  <> 'None'
          and attributes->'internal.sf_contract_id'  like sf_dcontract__c.salesforce_contract_id__c"
  
  res <- tbl(db,sql(q))
  return (res)
}

get_salesforce_reportouts <- function (db) {
  q <- "select domain.name as dname, sf_dcontract__c.salesforce_contract_id__c, sf_project_report_out__c.*
          from domain, sf_dcontract__c, sf_project_report_out__c
          where attributes ? 'internal.sf_contract_id' 
          and attributes->'internal.sf_contract_id'  <> ''
          and attributes->'internal.sf_contract_id'  <> 'None'
          and attributes->'internal.sf_contract_id'  like sf_dcontract__c.salesforce_contract_id__c
          and sf_dcontract__c.id like sf_project_report_out__c.contract__c"
  res <- tbl(db,sql(q))
  return (res)
  
}

get_impact123_case_properties <- function(db, domain_name){
  q <- sprintf("select domain.name as domain_name, users.user_id, form.form_id, form.time_end, 
          cases.case_id, cases.case_type, case_event.id
          from case_event, form, users, cases, domain
          where case_event.case_id = cases.id
          and case_event.form_id = form.id
          and form.domain_id = domain.id
          and form.user_id = users.id and domain.name like '%s'", domain_name)
  res <- tbl(db,sql(q))
  
  case_properties_q <- sprintf("select id, (each(case_properties)).* from case_event 
  where form_id in (select id from form 
                               where domain_id = (select id from domain where name like '%s'))",domain_name)
  case_properties <- collect(tbl(db,sql(case_properties_q)))
  if (NROW(case_properties) > 0) {
    case_properties_wide<-dcast(unique(case_properties), id ~ key, value.var="value")
    res<-merge(res,case_properties_wide,by="id",all=T)
  }
  
  return (res)
}

# returns visit table for use in visit detail data source
get_visit_detail_table <- function (db, limit=-1) {
  con <- db$con
  query <- 'select visit.id, count(form.id) as num_forms, visit.time_start, visit.time_end, users.id as user_pk,
            users.user_id, domain.name as domain
            from form, visit, users, domain
            where form.visit_id = visit.id and visit.user_id = users.id and form.domain_id = domain.id
            group by visit.id, visit.time_start, visit.time_end, users.id, users.user_id, domain.name'
  
  with_limit <-(limit > 0) 
  if (with_limit) {
    query <- paste0(query, ' limit ',limit )
  }
  
  v <- do_query(con, query)
  visit_ids <- paste(v$id,collapse=",")
  
  if (with_limit) { limit_clause <- sprintf(" where visit_id in (%s) ", visit_ids)} else {limit_clause <- ""}
  total_forms_q <- sprintf("select visit_id as id, count (distinct id) as total_forms from form %s 
                           group by visit_id", limit_clause)
  total_forms_res <- do_query(con, total_forms_q)
  v <- merge(v,total_forms_res,by.x="id",by.y="id")
  
  time_since_prev_q <- "select visit.id,  date_part('epoch',visit.time_start - lag(visit.time_end, 1) 
                        over (partition by visit.user_id order by visit.time_start)) as time_since_previous_hv
                        from visit inner join form on (form.visit_id = visit.id)  left join formdef
                        on (formdef.id = form.formdef_id)
                        where attributes->'Travel visit' = 'Yes' order by visit.user_id, visit.time_start"
  time_since_prev_res <- do_query(con, time_since_prev_q)
  v <- merge(v,time_since_prev_res,by.x="id",by.y="id",all.x=T,all.y=F)
  
  if (with_limit) { limit_clause <- sprintf(" and visit.id in (%s) ", visit_ids)} else {limit_clause <- ""}
  total_form_durations_q <- sprintf("select visit.id, 
                            extract('epoch' from sum(form.time_end - form.time_start)) as form_duration
                            from form, visit where form.visit_id = visit.id %s
                            group by visit.id",limit_clause)
  total_form_durations_res <- do_query(con, total_form_durations_q)
  v <- merge(v,total_form_durations_res,by.x="id",by.y="id")
  
  if (with_limit) { limit_clause <- sprintf(" where visit.id in (%s) ", visit_ids)} else {limit_clause <- ""}
  home_visits_q <- sprintf("select form.visit_id, bool_or(CASE WHEN (attributes->'Travel visit' = 'No') THEN FALSE 
                            WHEN (attributes->'Travel visit' = 'Yes') THEN TRUE ELSE null END) as home_visit
                            from visit inner join form on (form.visit_id = visit.id)  left join formdef
                            on (formdef.id = form.formdef_id) %s
                            group by form.visit_id",limit_clause)
  home_visits_res <- do_query(con, home_visits_q)
  v <- merge(v,home_visits_res,by.x="id",by.y="visit_id")
  
  return(v)
  
}

# interaction table query (one row for each visit to a case, visit to two cases = two rows)
# post-processed into the interaction table data source in data_sources.R get_interactions
# limit param can be used to limit the number of results returned
get_interaction_table <- function (db, limit=-1) {
  con <- db$con
  with_limit <-(limit > 0) 
  
  if (with_limit) { limit_clause <- sprintf(" (select * from visit limit %d) as vis ", limit)} else {limit_clause <- "visit as vis"}
  visit_q <- sprintf("select domain.name as domain, vis.id, users.id as user_pk,users.user_id, vis.time_start, vis.time_end 
              from %s, users, domain
              where vis.user_id = users.id and vis.domain_id = domain.id", limit_clause)
  visit_res <- do_query(con, visit_q)
  visit_ids <- paste(visit_res$id,collapse=",")
  
  if (with_limit) { limit_clause <- sprintf(" and form.visit_id in (%s) ", visit_ids)} else {limit_clause <- ""}
  interactions_q <- sprintf("select cases.case_id, form.visit_id, bool_or(case_event.created) as created, 
                    bool_or(case_event.updated) as updated, bool_or(case_event.closed) as closed
                    from cases, case_event, form 
                    where cases.id = case_event.case_id and form.id = case_event.form_id
                    %s
                    group by cases.case_id, form.visit_id",limit_clause)
  interactions_res <- do_query(con, interactions_q)
  inter <- merge(visit_res,interactions_res,by.x="id",by.y="visit_id")
  
  case_followup_query <- 'with case_visits as (select cases.case_id, cases.case_type,visit.id as visit_id, 
                                visit.time_start
                                from cases, form, case_event, visit
                               where case_event.case_id = cases.id
                               and case_event.form_id = form.id
                               and form.visit_id = visit.id
                               group by cases.case_id, cases.case_type, visit.id, visit.time_start)
                               select case_visits.case_id, case_visits.case_type, case_visits.visit_id, 
                               lag(case_visits.time_start,1) over (partition by case_visits.case_id 
                               order by case_visits.time_start) as prev_visit_start
                               from case_visits'
  
  case_followup_res <- do_query(con, case_followup_query)
  inter <- merge(inter,case_followup_res,by.x=c("id", "case_id"),by.y=c("visit_id","case_id"))
  
  return(inter)
}

# domain, user, date and device type for every form
# post-processed to device type data source in data_sources.R get_device_type
# limit param can be used to limit the number of results returned
get_device_type_table <- function (db, limit=-1) {
if (limit > 0) { limit_clause <- sprintf(" (select * from form limit %d) as frm ", limit)} else {limit_clause <- " form as frm "}
query <- paste("select domain.name as domain, users.id as user_pk,users.user_id, frm.time_start, to_char(frm.time_start, 'Mon YYYY') as month, 
                    CASE WHEN device_id ilike 'commconnect' THEN 'sms'
                    WHEN app_version LIKE '%Nokia%' THEN 'nokia'
                    WHEN app_version LIKE '%ODK%' THEN 'android'
                    WHEN app_version LIKE '2.0' THEN 'cloudcare'
                    WHEN app_version is null THEN 'none' 
                    ELSE 'other' END as device
                    from ",limit_clause,", users, domain
                    where frm.user_id = users.id
                    and frm.domain_id = domain. id", collapse=" ")

res <- tbl(db,sql(query))
return(res)
}

# user_id, username, user_type (web/mobile), is_superuser for all users
get_user_table <- function(db){
  mobile_users_q <- "select users.id as user_pk, users.user_id, users.username, 
            mobile_user.deactivated, mobile_user.deleted
            from users, mobile_user
            where users.id = mobile_user.user_pk"
  mobile_users <- collect(tbl(db,sql(mobile_users_q)))
  mobile_users$user_type <- 'mobile'
  mobile_users$is_superuser <- NA
  
  web_users_q <- "select users.id as user_pk, users.user_id, users.username, web_user.is_superuser
            from users, web_user
            where users.id = web_user.user_pk"
  web_users <- collect(tbl(db,sql(web_users_q)))
  web_users$user_type <- 'web'
  web_users$deactivated <- NA
  web_users$deleted <- NA
  
  res <- rbind(mobile_users, web_users)
  return(res)
}

# domain, user_id, first submission in that domain for all users
get_first_submission <- function(db){
  q <- "select domain.name, users.user_id, min(time_start) from domain, form, users
        where form.user_id = users.id
        and form.domain_id = domain.id
        group by users.user_id, domain.name"
  res <- tbl(db,sql(q))
  return (res)
}
  
get_device_log_table <- function(db, limit){
  query <- 'select device_log.id, log_date, to_char(log_date, \'Mon YYYY\') as "month.index", log_type, 
                    msg, domain.name as domain, users.id as user_pk,users.user_id
                    from device_log left outer join users on device_log.user_id = users.id 
                    inner join domain on device_log.domain_id = domain.id'
  if (limit > 0) {query <- paste0(query,' limit ', limit)}
  logs <- tbl(db, sql(query))
  return(logs)
}