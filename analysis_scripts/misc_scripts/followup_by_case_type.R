library(dplyr)
library(lubridate)

# load system conf
source(file.path("function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))
system_conf <- get_system_config(file.path("config_system.json"))

# get db connection
db <- get_db_connection(system_conf)

domain_name <- 'crs_remind'
case_actions_query <- sprintf('select users.user_id, form.visit_id,
cases.case_id, case_event.created as created, case_event.updated as updated, case_event.closed as closed 
from users, cases, case_event, form
                              where cases.domain_id = (select id from domain where name like \'%s\')
                              and users.id = cases.user_id
                              and case_event.case_id = cases.id
                              and case_event.form_id = form.id
                              group by users.user_id, form.visit_id, cases.case_id', domain_name)

case_followup_query <- sprintf('with case_visits as (select cases.case_id, cases.case_type,visit.id as visit_id, 
                                visit.time_start
                                from cases, form, case_event, visit
                               where cases.domain_id = (select id from domain where name like \'%s\')
                               and case_event.case_id = cases.id
                               and case_event.form_id = form.id
                               and form.visit_id = visit.id
                               group by cases.case_id, cases.case_type, visit.id, visit.time_start)
                               select case_visits.case_id, case_visits.case_type, case_visits.visit_id, case_visits.time_start, 
                               lag(case_visits.time_start,1) over (partition by case_visits.case_id order by case_visits.time_start) as prev_visit_start
                               from case_visits', domain_name)


actions <- tbl(db,sql(case_actions_query)) # this is a dplyr object representing the query
actions_df <- collect(actions) # collect runs the query and puts the result in a dateframe

followups <- tbl(db,sql(case_followup_query)) # this is a dplyr object representing the query
followups_df <- collect(followups) # collect runs the query and puts the result in a dateframe

merged_df <- merge(actions_df,followups_df, on = c('case_id','visit_id'))




