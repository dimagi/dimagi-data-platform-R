#Modified code
visit_pull_times <- function (db, limit=-1) {
  con <- db$con
  query <- 'select visit.id, visit.time_start, visit.time_end
  from form, visit
  where form.visit_id = visit.id
  group by visit.id, visit.time_start, visit.time_end'
  
  with_limit <-(limit > 0) 
  if (with_limit) {
    query <- paste0(query, ' limit ',limit )
  }
  
  v <- do_query(con, query)
  return(v)
}

form_pull_times <- function (db, limit=-1) {
  con <- db$con
  query <- 'select form.id, form.time_start, form.time_end
  from form'
  
  with_limit <-(limit > 0) 
  if (with_limit) {
    query <- paste0(query, ' limit ',limit )
  }
  
  f <- do_query(con, query)
  return(f)
}




#Original code
get_data_source <- function (db, table_name, limit=-1) {
  tryCatch({
    if (limit>0) {
      query<-build_sql('SELECT * FROM ', ident(table_name),' limit ', as.integer(limit))
    }
    else {
      query<-build_sql('SELECT * FROM ', ident(table_name))
    }
    return(tbl(db, sql(query)))
  }, error = function(err) {
    s <- do.call(sprintf("get_%s",table_name),args=list(db, limit))
    return(s)
  })
}

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



