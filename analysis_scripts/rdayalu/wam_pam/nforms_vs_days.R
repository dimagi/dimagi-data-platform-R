library(ggplot2)

#Import app table, calculate amplifies_workers and merge to domain table.
domain <- select(domain, name, domain_id, domain_has_amplifies_workers)

#all_monthly <- monthly_table
#Run WAM code through line 159

all_monthly$nforms_ge_0 <- all_monthly$nforms >= 0
all_monthly$nforms_ge_5 <- all_monthly$nforms >= 5
all_monthly$nforms_ge_10 <- all_monthly$nforms >= 10
all_monthly$nforms_ge_15 <- all_monthly$nforms >= 15
all_monthly$nforms_ge_20 <- all_monthly$nforms >= 20
all_monthly$nforms_ge_25 <- all_monthly$nforms >= 25
all_monthly$nforms_ge_30 <- all_monthly$nforms >= 30
all_monthly$nforms_ge_35 <- all_monthly$nforms >= 35
all_monthly$nforms_ge_40 <- all_monthly$nforms >= 40
all_monthly$nforms_ge_45 <- all_monthly$nforms >= 45
all_monthly$nforms_ge_50 <- all_monthly$nforms >= 50
all_monthly$wam_using <- all_monthly$active_days >= 4

sum(all_monthly$nforms_ge_50)
sum(all_monthly$nforms_ge_50)/nrow(all_monthly)

all_monthly$nforms_ge_5_and_using <- all_monthly$nforms_ge_5 == all_monthly$wam_using
all_monthly$nforms_ge_10_and_using <- all_monthly$nforms_ge_10 == all_monthly$wam_using
all_monthly$nforms_ge_15_and_using <- all_monthly$nforms_ge_15 == all_monthly$wam_using
all_monthly$nforms_ge_20_and_using <- all_monthly$nforms_ge_20 == all_monthly$wam_using
all_monthly$nforms_ge_25_and_using <- all_monthly$nforms_ge_25 == all_monthly$wam_using
all_monthly$nforms_ge_30_and_using <- all_monthly$nforms_ge_30 == all_monthly$wam_using
all_monthly$nforms_ge_35_and_using <- all_monthly$nforms_ge_35 == all_monthly$wam_using
all_monthly$nforms_ge_40_and_using <- all_monthly$nforms_ge_40 == all_monthly$wam_using
all_monthly$nforms_ge_45_and_using <- all_monthly$nforms_ge_45 == all_monthly$wam_using
all_monthly$nforms_ge_50_and_using <- all_monthly$nforms_ge_50 == all_monthly$wam_using

sum(all_monthly$nforms_ge_50_and_using)
sum(all_monthly$nforms_ge_50_and_using)/nrow(all_monthly)

#Add columns for true/false positives/negatives for a certain threshold T
all_monthly$true_positive <- all_monthly$nforms_ge_15 == T & all_monthly$wam_using == T
all_monthly$false_positive <- all_monthly$nforms_ge_15 == T & all_monthly$wam_using == F
all_monthly$true_negative <- all_monthly$nforms_ge_15 == F & all_monthly$wam_using == F
all_monthly$false_negative <- all_monthly$nforms_ge_15 == F & all_monthly$wam_using == T

all_monthly$true_positive <- all_monthly$nforms_ge_20 == T & all_monthly$wam_using == T
all_monthly$false_positive <- all_monthly$nforms_ge_20 == T & all_monthly$wam_using == F
all_monthly$true_negative <- all_monthly$nforms_ge_20 == F & all_monthly$wam_using == F
all_monthly$false_negative <- all_monthly$nforms_ge_20 == F & all_monthly$wam_using == T

#all_monthly$unique_int <- 1:nrow(all_monthly)
check_nforms <- all_monthly %>% group_by(domain) %>% 
  summarise(nuser_months = length(unique(unique_int)), 
            nforms_20_equal_using = sum(nforms_ge_20_and_using, na.rm=T), 
            true_positive = sum(true_positive), 
            false_positive = sum(false_positive), 
            true_negative = sum(true_negative), 
            false_negative = sum(false_negative))
check_nforms$per_equal_20 <- (check_nforms$nforms_20_equal_using/check_nforms$nuser_months)*100
check_nforms$per_true_pos <- (check_nforms$true_positive/check_nforms$nuser_months)*100
check_nforms$per_false_pos <- (check_nforms$false_positive/check_nforms$nuser_months)*100
check_nforms$per_true_neg <- (check_nforms$true_negative/check_nforms$nuser_months)*100
check_nforms$per_false_neg <- (check_nforms$false_negative/check_nforms$nuser_months)*100

#Merge to check_nforms
check_nforms <- merge(check_nforms, domain, by.x = "domain", by.y = "name", all.x = T)
check_nforms <- arrange(check_nforms, per_equal_20)
write.csv(check_nforms, file = "nforms_T_20.csv", row.names=F)


#test_15 <- filter(check_nforms, domain_has_amplifies_workers == T)

#Graphics

all_monthly$cond <- 1

ggplot(all_monthly, aes(x=cond, y=active_days)) + 
  geom_boxplot(width=1) + 
  stat_summary(fun.y=mean, geom="point", shape=5, size=4)

ggplot(all_monthly, aes(x=cond, y=nforms)) + 
  geom_boxplot(width=1) + 
  coord_cartesian(ylim=c(0, 200)) + 
  stat_summary(fun.y=mean, geom="point", shape=5, size=4)


ggplot(check_nforms, aes(x=per_equal_15)) + 
  geom_density()

ggplot(all_monthly, aes(x=active_days, y=nforms)) +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=FALSE) + 
  scale_y_continuous(limits=c(0, 50))

########################################################################################################


all_monthly$nforms_log <- log10(all_monthly$nforms)
all_monthly$active_days_log <- log10(all_monthly$active_days)

ggplot(all_monthly, aes(x=nforms)) + 
  geom_density() + 
  scale_x_continuous(limits=c(0, 200))

ggplot(all_monthly, aes(x=nforms_log)) + 
  geom_density()

ggplot(all_monthly, aes(x=active_days)) + 
  geom_density() + 
  scale_x_continuous(limits=c(0, 31))

ggplot(all_monthly, aes(x=active_days_log)) + 
  geom_density()