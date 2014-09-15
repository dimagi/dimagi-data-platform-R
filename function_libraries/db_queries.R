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

# returns all attributes for domains in domain_list, sector and subsector names as lists
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
              array(select sector.name 
              from sector, domain_sector 
              where domain_sector.domain_id = domain.id 
              and domain_sector.sector_id=sector.id) as sector
              from domain
              order by domain.name"
  sectors <- do_query(con,sectors_q)
  sectors<-transform(sectors, sector = strsplit(substr(sector,2,nchar(sector)-1),split=","))
  sectors$sector[sapply(sectors$sector,length)==0]<-NA
  retframe<-merge(retframe,sectors,by="name",all=T)
  
  subsectors_q <- "select domain.name, 
              array(select subsector.name 
              from subsector, domain_subsector 
              where domain_subsector.domain_id = domain.id 
              and domain_subsector.subsector_id=subsector.id) as subsector
              from domain
              order by domain.name"
  subsectors <- do_query(con,subsectors_q)
  subsectors<-transform(subsectors, subsector = strsplit(substr(subsector,2,nchar(subsector)-1),split=","))
  subsectors$subsector[sapply(subsectors$subsector,length)==0]<-NA
  retframe<-merge(retframe,subsectors,by="name",all=T)
  
  retframe <- data.frame(retframe,check.names=T)
  return(retframe)
}

# interaction table (one row for each visit to a case, visit to two cases = two rows)
# limit param can be used to limit the number of results returned
# todo convert to use dplyr? no good way to do the limit though.
get_interaction_table <- function (db, limit=-1) {
  con <- db$con
  with_limit <-(limit > 0) 
  
  if (with_limit) { limit_clause <- sprintf(" (select * from visit limit %d) as vis ", limit)} else {limit_clause <- "visit as vis"}
  visit_q <- sprintf("select domain.name as domain, vis.id, users.user_id, vis.time_start, vis.time_end 
              from %s, users, domain 
              where vis.user_id = users.id and users.domain_id = domain.id", limit_clause)
  visit_res <- do_query(con, visit_q)
  visit_ids <- paste(visit_res$id,collapse=",")
  
  if (with_limit) { limit_clause <- sprintf(" and form.visit_id in (%s) ", visit_ids)} else {limit_clause <- ""}
  interactions_q <- sprintf("select cases.case_id, form.visit_id from cases, case_event, form 
                    where cases.id = case_event.case_id and form.id = case_event.form_id
                    %s
                    group by cases.case_id, form.visit_id",limit_clause)
  interactions_res <- do_query(con, interactions_q)
  v <- merge(visit_res,interactions_res,by.x="id",by.y="visit_id")
  
  if (with_limit) { limit_clause <- sprintf(" where visit_id in (%s) ", visit_ids)} else {limit_clause <- ""}
  total_forms_q <- sprintf("select visit_id as id, count (distinct id) as total_forms from form %s 
                           group by visit_id", limit_clause)
  total_forms_res <- do_query(con, total_forms_q)
  v <- merge(v,total_forms_res,by.x="id",by.y="id")
  
  # this one needs to do the lag over all visits unfortunately
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
                            on (formdef.xmlns = form.xmlns and formdef.app_id = form.app_id) %s
                            group by form.visit_id",limit_clause)
  home_visits_res <- do_query(con, home_visits_q)
  v <- merge(v,home_visits_res,by.x="id",by.y="visit_id")

return(v)
}

# domain, user, date and device type for every form
# limit param can be used to limit the number of results returned
get_device_type_table <- function (db, limit=-1) {
if (limit > 0) { limit_clause <- sprintf(" (select * from form limit %d) as frm ", limit)} else {limit_clause <- " form as frm "}
query <- paste("select domain.name as domain, users.user_id, frm.time_start, to_char(frm.time_start, 'Mon YYYY') as month, 
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