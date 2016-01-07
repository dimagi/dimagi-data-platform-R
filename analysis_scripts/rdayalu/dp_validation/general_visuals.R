#Visualize tenure on CC

month_count <- master_set %.%
  group_by(domain, user_id) %.%
  summarise(months_on_cc = length(unique(calendar_month)))

month_count <- tula_typical %.%
  group_by(domain, user_id) %.%
  summarise(months_on_cc = length(unique(calendar_month)))

#Histogram
# Include all observations
myhist <- ggplot(n_apps, aes(x=n_apps)) + 
  geom_histogram(binwidth=1, colour="black", fill="lightblue") + 
  geom_vline(aes(xintercept=median(n_apps, na.rm=T)),
             color="red", linetype="dashed", size=1)

cumulative_dist <- plot(ecdf(month_count$months_on_cc))