get_active_users <- function(dt) {
  user_interaction <- get_last_interaction(dt)
  inactive_line <- get_inactive_line(export_date, 30)
  user_interaction$active <- ifelse(as.Date(user_interaction$last_interaction) >= as.Date(inactive_interaction_line), "yes", "no")
  print(table(user_interaction$active))  
}

