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

# returns visit table for use in visit detail data source
get_visit_detail_table <- function (db, limit=-1) {
  con <- db$con
  query <- 'select visit.id, count(form.id) as num_forms, visit.time_start, visit.time_end, users.id as user_pk,users.user_id, domain.name as domain
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
  
  time_since_prev_q <- "select visit.id,  date_part('epoch',time_start - lag(time_end, 1) over (partition by visit.user_id order by time_start)) as time_since_previous from visit order by visit.user_id, time_start"
  time_since_prev_res <- do_query(con, time_since_prev_q)
  v <- merge(v,time_since_prev_res,by.x="id",by.y="id",all.x=T,all.y=F)
  
  if (with_limit) { limit_clause <- sprintf(" and visit.id in (%s) ", visit_ids)} else {limit_clause <- ""}
  total_form_durations_q <- sprintf("select visit.id, extract('epoch' from sum(form.time_end - form.time_start)) as form_duration
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
              where vis.user_id = users.id and users.domain_id = domain.id", limit_clause)
  visit_res <- do_query(con, visit_q)
  visit_ids <- paste(visit_res$id,collapse=",")
  
  if (with_limit) { limit_clause <- sprintf(" and form.visit_id in (%s) ", visit_ids)} else {limit_clause <- ""}
  interactions_q <- sprintf("select cases.case_id, form.visit_id, case_event.created, case_event.updated, case_event.closed
                    from cases, case_event, form 
                    where cases.id = case_event.case_id and form.id = case_event.form_id
                    %s
                    group by cases.case_id, form.visit_id, case_event.created, case_event.updated, case_event.closed",limit_clause)
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
                               lag(case_visits.time_start,1) over (partition by case_visits.case_id order by case_visits.time_start) as prev_visit_start
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
                    CASE WHEN app_version LIKE '%Nokia%' THEN 'nokia'
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

# domain, user_id, username, first_submission date for all users
get_user_table <- function(db){
  query <- "select domain.name as domain, users.id as user_pk,users.user_id, users.username, min(form.time_start) as first_submission
            from domain, users, form
            where users.domain_id = domain.id
            and form.user_id = users.id
            group by domain.name, users.id as user_pk,users.user_id, users.username"
  res <- tbl(db,sql(query))
  return(res)
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