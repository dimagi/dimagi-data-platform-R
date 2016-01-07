# 12/15/14
# Correlation matrix for full blog dataset
# 57,207 observations
library(corrplot)

# Make datset with only the following variable of interest
indicators_short = c("nvisits", "active_day_percent", "nforms", "median_visit_duration", 
               "median_visits_per_day", "time_using_cc", "ncases_registered", 
               "register_followup", "case_register_followup_rate", 
               "ncases_touched", "nunique_followups", "ninteractions")
corr_set <- fullset
corr_set <- corr_set[,names(corr_set) %in% indicators_short]
names(corr_set) <- c("# visits", "% active days", "# forms", "visit duration", "visits/day", 
                     "time using cc", "# interactions", "# cases registered", "# follow-up visits",  
                     "% follow-up visits", "# cases", "# cases followed-up")

#Generate numerical correlation matrix
num_corr <- cor(corr_set, use="complete.obs")
num_corr <- round(num_corr, digits = 2)

#Generate visual correlation plot
corrplot(num_corr, tl.srt=45, tl.cex = 0.6, 
         diag=F, type={"upper"}, order = "AOE")
cor_plot <- corrplot(num_corr, tl.srt=45, tl.cex = 0.6, 
         diag=F, order = "AOE")



