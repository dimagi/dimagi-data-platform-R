#Pull visit times or form times from the main visit or form table using an offset parameter (per Yedi). 
#This ensures that the visit/form date/time stamps are pulled correctly without dropping the time component.

#Number of rows in visit table through June 2015 = 7,890,421
#Time component drops with offset > 0 and limit approximately > 10,000
#test <- visit_pull_times(db)

#Form table has 12,787,766 rows through June 2015

#Function to pull visit times from the visit table using offset parameter
visit_pull_times <- function (db, limit=-1, offset=-1) {
  con <- db$con
  query <- 'select visit.id, visit.time_start, visit.time_end
  from form, visit
  where form.visit_id = visit.id
  group by visit.id, visit.time_start, visit.time_end'
  
  with_limit <-(limit > 0) 
  if (with_limit) {
    query <- paste0(query, ' limit ',limit )
  }
  with_offset <-(offset > 0) 
  if (with_limit & with_offset) {
    query <- paste0(query, ' offset ',offset )
  }
  
  
  v <- do_query(con, query)
  return(v)
}

#Pull visit table time stamps using defined offset and limit parameters
offset_vec <- c(-1, seq(10000, 7890000, by = 10000))

for (i in 1:length(offset_vec)) {
  new_batch <- visit_pull_times(db, limit = 10000, offset = offset_vec[i])
  
  if (i == 1) {
    visit_times <- new_batch
  }
  
  if (i > 1) {
    visit_times <- rbind(visit_times, new_batch)
  }
}