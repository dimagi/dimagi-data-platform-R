#Indicator development
#First load a sample of the interactions table (first 1K rows or so)
#Then pass this interactions table as x to test the indicator functions you write below

#For the interaction table "id" = visit id. This will be the same for two different rows if two 
#related cases were updated. Only the case_id will be different. time_end and time_start will be the 
#same too because the whole interaction will apply to both cases.

nvisits <- function(x) length(unique(x$id))

#Median visit duration: if there are two cases visited in the same visit, we don't want to count
#that interaction twice in our calculation for median visit duration, so we need to group by "id" first
#and then operate on that new table
median_visit_duration <- function(x) as.numeric(median((x$time_end - x$time_start)/ 60, na.rm=TRUE))
time_using_cc <- function(x) sum(x$form_duration, na.rm = T)  

nvisits_travel <- function(x) sum(x$home_visit, na.rm=T)
nvisits_travel_batch <- function(x) sum(x$time_since_previous_hv/60<10, na.rm = T)  
travel_batch_percent <- function(x) (nvisits_travel_batch(x) / nvisits_travel(x))*100 

