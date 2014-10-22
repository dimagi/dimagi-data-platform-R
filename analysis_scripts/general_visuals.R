#Visualize tenure on CC

month_count <- master_set %.%
  group_by(domain, user_id) %.%
  summarise(months_on_cc = length(unique(calendar_month)))

month_count <- tula_typical %.%
  group_by(domain, user_id) %.%
  summarise(months_on_cc = length(unique(calendar_month)))

#Histogram
# Include all observations
myhist <- ggplot(month_count, aes(x=months_on_cc)) + 
  geom_histogram(binwidth=1, colour="black", fill="lightblue") + 
  geom_vline(aes(xintercept=median(months_on_cc, na.rm=T)),
             color="red", linetype="dashed", size=1)

cumulative_dist <- plot(ecdf(month_count$months_on_cc))