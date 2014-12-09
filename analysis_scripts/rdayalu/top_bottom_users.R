#User consistency blog
#ID top/bottom 10% ncases_touched
#Only keep domains with nusers >= 30

#nusers <- test2_data %>% group_by(domain) %>% 
#  summarise(nusers = length(unique(user_pk)))
nusers <- test2_data %>% group_by(domain, calendar_month) %>% 
  summarise(nusers = length(unique(user_pk)))
nusers <- filter(nusers, nusers >= 30)
nusers$concat <- paste(nusers$domain, nusers$calendar_month, sep = "_")

test2_data$concat <- paste(test2_data$domain, test2_data$calendar_month, sep = "_")
tb <- test2_data[test2_data$concat %in% nusers$concat,]
#tb <- test2_data[test2_data$domain %in% nusers$domain,]

#Percentile function
percentile <- function(x) ecdf(x)(x)
  raw_percentile <- tb %>%
    group_by(domain, calendar_month) %>%
    mutate(percentile_ntouched = percentile(ncases_touched)*100)

#Mark top/bottom 10% users
raw_percentile$top_10p_ntouched <- raw_percentile$percentile_ntouched >= 90
raw_percentile$bot_10p_ntouched <- raw_percentile$percentile_ntouched <= 10
top_users <- unique(filter(raw_percentile, top_10p_ntouched == T)$user_pk)
bot_users <- unique(filter(raw_percentile, bot_10p_ntouched == T)$user_pk)
true_top <- top_users[!(top_users %in% bot_users)]
true_bot <- bot_users[!(bot_users %in% top_users)]

#Create dataset for exclusively top/bottom users
top <- test2_data[test2_data$user_pk %in% true_top,]
bot <- test2_data[test2_data$user_pk %in% true_bot,]
#top <- test2_data[test2_data$user_pk %in% top_users,]
#bot <- test2_data[test2_data$user_pk %in% bot_users,]

#Create dataset for top/bottom observations
top <- filter(raw_percentile, top_10p_ntouched == T)
bot <- filter(raw_percentile, bot_10p_ntouched == T)

#Calculate correlation
cor(top$prev_ncases_touched, top$ncases_touched, use = "complete.obs")
cor(bot$prev_ncases_touched, bot$ncases_touched, use = "complete.obs")
#Correlation function
correlation <- function(x, y) cor(x, y, use = "complete.obs")
corr_domains <- tb %>%
  group_by(domain) %>%
  summarise(corr_ntouched = correlation(prev_ncases_touched, ncases_touched))
corr_domains <- arrange(corr_domains, corr_ntouched)
top_domains <- top %>%
  group_by(domain) %>%
  summarise(corr_top_ntouched = correlation(prev_ncases_touched, ncases_touched))

bot_domains <- bot %>%
  group_by(domain) %>%
  summarise(corr_bot_ntouched = correlation(prev_ncases_touched, ncases_touched))
detach("package:data.table")
top_domains <- merge(top_domains, bot_domains, by = "domain", all.x = T)
top_domains <- arrange(top_domains, corr_top_ntouched)

#Scatterplots

tb <- filter(tb, domain_numeric == 18 | domain_numeric == 264)
tb <- select(test, domain_numeric, user_pk, calendar_month, prev_ncases_touched, ncases_touched)

test <- filter(tb, domain_numeric == 18)
g <- ggplot(test, aes(x=prev_ncases_touched, y=ncases_touched)) +
  geom_point(shape=1) +
  scale_x_continuous(limits=c(0,50)) +
  scale_y_continuous(limits=c(0,50)) +
  geom_smooth(method=lm) +
  annotate("text", label="r^2 == 0.75", parse = T, x=38, y=3)

test <- filter(tb, domain_numeric == 264)
g <- ggplot(test, aes(x=prev_ncases_touched, y=ncases_touched)) +
  geom_point(shape=1) +
  scale_x_continuous(limits=c(0,50)) +
  scale_y_continuous(limits=c(0,50)) +
  geom_smooth(method=lm) + 
  annotate("text", label="r^2 == 0.36", parse = T, x=38, y=3)

write.csv(test, file = "domain_consistency_comparison.csv")

#Top 10% and bottom 10% scatterplots
g <- ggplot(top, aes(x=prev_ncases_touched, y=ncases_touched)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + 
  annotate("text", label="r^2 == 0.69", parse = T, x=6, y=98)

g <- ggplot(bot, aes(x=prev_ncases_touched, y=ncases_touched)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  scale_y_continuous(limits=c(0,50)) +
  scale_x_continuous(limits=c(0,50)) +
  annotate("text", label="r^2 == 0.34", parse = T, x=3, y=48)

#Test statistic for correlations
cor.test(top$prev_ncases_touched, top$ncases_touched, 
         alternative = "two.sided", conf.level = 0.95)
cor.test(bot$prev_ncases_touched, bot$ncases_touched, 
         alternative = "two.sided", conf.level = 0.95)

test <- test2_data[!(test2_data$user_pk %in% top_users | 
                       test2_data$user_pk %in% bot_users),]
cor(test$prev_ncases_touched, test$ncases_touched, use = "complete.obs")
cor.test(test$prev_ncases_touched, test$ncases_touched, 
         alternative = "two.sided", conf.level = 0.95)