library(RPostgreSQL)
library(reshape2)

get_con <- function(user,pass,host,port, dbname) {
drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, dbname=dbname,
                 user=user,
                 pass=pass,
                 host=host, 
                 port=port)
return(con)
}

# returns all attributes for domains in domain_list, sector and subsector names as lists
get_domain_table <- function (con) {
  normal_cols_q <- sprintf("select * from domain  
                 order by name")
  rs <- dbSendQuery(con,normal_cols_q)
  normal_cols <- fetch(rs,n=-1)
  dbClearResult(rs)
  retframe<-normal_cols[, !(colnames(normal_cols) %in% c("attributes"))]
  
  hstore_keyvalues_q <- sprintf("select name, (each(attributes)).* from domain 
                 order by name")
  rs <- dbSendQuery(con,hstore_keyvalues_q)
  hstore_keyvalues <- fetch(rs,n=-1)
  dbClearResult(rs)
  hstore_wide<-dcast(hstore_keyvalues, name ~ key)
  retframe<-merge(retframe,hstore_wide,by="name")
  
  sectors_q <- sprintf("select domain.name, 
              array(select sector.name 
              from sector, domain_sector 
              where domain_sector.domain_id = domain.id 
              and domain_sector.sector_id=sector.id) as sector_arr
              from domain
              order by domain.name")
  rs <- dbSendQuery(con,sectors_q)
  sectors <- fetch(rs,n=-1)
  dbClearResult(rs)
  sectors<-transform(sectors, sector_arr = strsplit(substr(sector_arr,2,nchar(sector_arr)-1),split=","))
  sectors$sector_arr[sapply(sectors$sector_arr,length)==0]<-NA
  retframe<-merge(retframe,sectors,by="name")
  
  subsectors_q <- sprintf("select domain.name, 
              array(select subsector.name 
              from subsector, domain_subsector 
              where domain_subsector.domain_id = domain.id 
              and domain_subsector.subsector_id=subsector.id) as subsector_arr
              from domain
              order by domain.name")
  rs <- dbSendQuery(con,subsectors_q)
  subsectors <- fetch(rs,n=-1)
  dbClearResult(rs)
  subsectors<-transform(subsectors, subsector_arr = strsplit(substr(subsector_arr,2,nchar(subsector_arr)-1),split=","))
  subsectors$subsector_arr[sapply(subsectors$subsector_arr,length)==0]<-NA
  retframe<-merge(retframe,subsectors,by="name")
  return(retframe)
}

# interaction table (one row for each visit to a case, visit to two cases = two rows)
# for all domains in list.
get_interaction_table <- function (con, domain_list) {
  domain_names_str <- paste (sprintf("'%s'", domain_list), collapse=",")
  query <- sprintf("with total_forms as 
                 (select visit_id as id, count (distinct id) as total_forms 
                 from form 
                 group by visit_id), 
                 time_sinces as 
                 (select visit.id,  date_part('epoch',time_start - lag(time_end, 1) 
                 over (partition by visit.user_id order by time_start)) as time_since_previous 
                 from visit 
                 order by visit.user_id, time_start),
                 total_form_durations as 
                 (select visit.id, extract('epoch' from sum(form.time_end - form.time_start)) as form_duration
                 from form, visit 
                 where form.visit_id = visit.id
                 group by visit.id),
                 home_visits as
                 (select form.visit_id, bool_or(CASE WHEN (attributes->'Travel visit' = 'No') THEN FALSE 
                 WHEN (attributes->'Travel visit' = 'Yes') THEN TRUE ELSE null END) as home_visit
                 from visit
                 inner join form on (form.visit_id = visit.id)
                 left join formdef
                 on (formdef.xmlns = form.xmlns
                 and formdef.app_id = form.app_id)
                 group by form.visit_id),
                 case_visits as (select visit.id as visit_id, cases.case_id from visit,form,case_event, cases
                 where visit.id = form.visit_id
                 and form.id = case_event.form_id
                 and cases.id = case_event.case_id
                 group by  visit.id, cases.case_id
                  )
                 select visit.id, users.user_id, case_visits.case_id, 
                 visit.time_start, visit.time_end, total_form_durations.form_duration, 
                 total_forms.total_forms, time_sinces.time_since_previous, home_visits.home_visit, 
                 domain.name as domain
                 from visit, users, domain,
                 total_forms,time_sinces, total_form_durations, home_visits, case_visits
                 where visit.user_id = users.id 
                 and total_forms.id = visit.id 
                 and time_sinces.id = visit.id
                 and total_form_durations.id = visit.id 
                 and home_visits.visit_id = visit.id
                 and case_visits.visit_id = visit.id
                 and users.domain_id = domain.id 
                 and domain.name in (%s) 
                 order by visit.user_id, time_start", domain_names_str)

rs <- dbSendQuery(con,query)
v <- fetch(rs,n=-1)
dbClearResult(rs)
return(v)
}

close_con <- function (con) {
dbDisconnect(con)
}