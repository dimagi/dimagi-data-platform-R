#NSF big data grant analysis and visuals
library(ggplot2)
library(gridExtra)
library(scales)
library(maps)
library(mapproj)

all_monthly <- monthly_table
#Change column names as needed
names(all_monthly)[names(all_monthly) == "month.index"] = "calendar_month"
names(all_monthly)[names(all_monthly) == "numeric_index"] = "month_index"
#Convert calendar month to actual date
all_monthly$calendar_month <- parse_date_time(paste('01', all_monthly$calendar_month), '%d %b %Y!')
all_monthly$calendar_month <- as.Date(all_monthly$calendar_month)
all_monthly$month_abbr <- month(all_monthly$calendar_month, label = T, abbr = T)

#Prepare domain_table for merging in domain facets
#Bring in sector information
sector <- tbl(db, "sector")
sector <- collect(sector)
names(sector)[names(sector) == "name"] = "sector_final"
domain_sector <- tbl(db, "domain_sector")
domain_sector <- collect(domain_sector)
domain_sector <- select(domain_sector, domain_id, sector_id)
domain_table <- merge(domain_table, domain_sector, by.x = "id", by.y = "domain_id", all.x = T)
domain_table <- merge(domain_table, sector, by.x = "sector_id", by.y = "id", all.x = T)
#Bring in subsector information
subsector <- tbl(db, "subsector")
subsector <- collect(subsector)
subsector <- select(subsector, id, name)
subsector <- filter(subsector, !is.na(name))
subsector <- filter(subsector, name != "")
names(subsector)[names(subsector) == "name"] = "subsector_final"
domain_subsector <- tbl(db, "domain_subsector")
domain_subsector <- collect(domain_subsector)
domain_subsector <- select(domain_subsector, domain_id, subsector_id)
domain_table <- merge(domain_table, domain_subsector, by.x = "id", by.y = "domain_id", all.x = T)
domain_table <- merge(domain_table, subsector, by.x = "subsector_id", by.y = "id", all.x = T)
#Consolidate country information
domain <- domain_table
is.na(domain$deployment.country) <- domain$deployment.country == ""
is.na(domain$country) <- domain$country == ""
domain$country_final <- domain$deployment.country
keep_country <- which(is.na(domain$deployment.country) & !is.na(domain$country))
domain$country_final[keep_country] <- domain$country[keep_country]

#Keep only columns of interest
names(domain)[names(domain) == "id"] = "domain_id"
facets_to_merge <- select(domain, name, domain_id, country_final, sector_final, 
                          subsector_final)

#Merge domain facets from domain table into all_monthly table
all_monthly <- merge(all_monthly, facets_to_merge, by.x = "domain", 
                     by.y = "name", all.x = T)

#------------------------------------------------------------------------#
#VOLUME
#Figure 1: # forms, # users, # cases
#2010 - 2014
monthly_10_14 <- filter(all_monthly, calendar_month <= "2014-12-01" & 
                          calendar_month >= "2010-01-01")

#cumulative # forms per month 
cum_forms <- monthly_10_14 %>% group_by(calendar_month) %>% 
  summarise (nforms = sum(nforms, na.rm = T))
cum_forms$cum_forms <- cumsum(cum_forms$nforms)

#active users per month
nusers_active <- monthly_10_14 %>% group_by(calendar_month) %>% 
  summarise(nusers = length(unique(user_id)))

#cases registered per month
cases_reg <- monthly_10_14 %>% group_by(calendar_month) %>% 
  summarise(ncases_reg = sum(ncases_registered, na.rm = T))

#Plot cases and forms on side-by-side graphs
cum_nforms <- ggplot(cum_forms, aes(x=calendar_month, y=cum_forms, group=1)) +
  geom_line(colour = "darkslateblue", size = 0.5) + 
  scale_y_continuous(labels = comma) + 
  xlab("Calendar Month") + 
  ylab("# forms (cumulative)") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size=6), 
        axis.title=element_text(size=6))

cases_visual <- ggplot(cases_reg, aes(x=calendar_month, y=ncases_reg, group=1)) +
  geom_line(colour = "coral1", size = 0.5) + 
  scale_y_continuous(labels = comma) + 
  xlab("Calendar Month") + 
  ylab("# of cases registered") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size=6), 
        axis.title=element_text(size=6))

grid.arrange(cum_nforms, cases_visual, nrow=2)

#------------------------------------------------------------------------#
#VELOCITY
#Median number of forms per day
forms_daily <- form_table %>% group_by(form_date) %>% 
  summarise(nforms = length(unique(id)),
            ncases = sum(ncases, na.rm = T),
            nusers = length(unique(user_pk)),
            ndomains = length(unique(domain_id)))
median(forms_daily$nforms)
median(forms_daily$ncases)


#------------------------------------------------------------------------#

#Figure 2: World map: # users by country
world_map <- map_data("world")

#Check country spellings in all_monthly
country_table <- data.frame(table(all_monthly$country_final, useNA = "always"))
names(country_table)[names(country_table) == "Var1"] = "country"
summary(country_table$country %in% world_map$region)
check_country <- which(!(country_table$country %in% world_map$region))

#Recode incorrect country names
all_monthly$country_final[all_monthly$country_final == "Burma"] <- "Myanmar"
all_monthly$country_final[all_monthly$country_final == "KENYA"] <- "Kenya"
all_monthly$country_final[all_monthly$country_final == "Nicaragu"] <- "Nicaragua"
all_monthly$country_final[all_monthly$country_final == "Niger and Burkina Faso"] <- "Niger"
all_monthly$country_final[all_monthly$country_final == "Philippines, Thailand"] <- "Philippines"
all_monthly$country_final[all_monthly$country_final == "United Kingdom"] <- "UK"
all_monthly$country_final[all_monthly$country_final == "United States"] <- "USA"
all_monthly$country_final[all_monthly$country_final == "United States of America"] <- "USA"
all_monthly$country_final[all_monthly$country_final == "bangladesh"] <- "Bangladesh"
all_monthly$country_final[all_monthly$country_final == "france"] <- "France"
all_monthly$country_final[all_monthly$country_final == "kenya"] <- "Kenya"
all_monthly$country_final[all_monthly$country_final == "malawi"] <- "Malawi"
all_monthly$country_final[all_monthly$country_final == "sdfg"] <- NA

#nusers per country
nusers <- all_monthly %>% group_by(country_final) %>% 
  summarise(nusers = length(unique(user_pk)))

#Remove rows with country = NA
nusers <- filter(nusers, !is.na(country_final))

#Merge with world_map
world_map <- merge(world_map, nusers, by.x = "region", by.y = "country_final", all.x = T)
world_map <- arrange(world_map, group, order)

#map <- ggplot(world_map, aes(x=long, y=lat, group=group, fill=nusers)) + 
#                geom_polygon(colour="black") + 
#                coord_map("polyconic")

theme_clean <- function(base_size=12) {
  require(grid)
  theme_grey(base_size) %+replace% 
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(), 
      panel.background = element_blank(), 
      panel.grid = element_blank(), 
      axis.ticks.length = unit(0, "cm"), 
      axis.ticks.margin = unit(0, "cm"), 
      panel.margin = unit(0, "lines"), 
      plot.margin = unit(c(0, 0, 0, 0), "lines"), 
      complete = T
      )
}

p <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group,
                                    fill = nusers)) +
  geom_path(colour="black") +
  scale_y_continuous(breaks=(-2:2) * 30) +
  scale_x_continuous(breaks=(-4:4) * 45) +
  scale_fill_distiller(palette="Oranges") +  
  labs(fill="Number of users") +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme_clean() + 
  theme(legend.title=element_text(size=10)) + 
  theme(legend.text=element_text(size=10))

#------------------------------------------------------------------------#

#Univariate density plots: use fullset
all_monthly$ndays_in_month <- days_in_month(all_monthly$calendar_month)
all_monthly$active_day_percent <- (all_monthly$active_days/all_monthly$ndays_in_month)*100

library(pastecs)
indicators_table <- cbind(all_monthly$nvisits, all_monthly$active_day_percent, all_monthly$nforms, 
                          all_monthly$time_using_cc, all_monthly$ncases_registered, 
                          all_monthly$ncases_touched, all_monthly$nunique_followups)
colnames(indicators_table) <- c("nvisits", "active_day_percent", "nforms", 
                                "time_using_cc", "ncases_registered", 
                                "ncases_touched", "nunique_followups")

options(scipen=100)
options(digits=2)
summary_stats <- as.matrix(stat.desc(indicators_table))

#Density plots with semi-transparent fill
g_nforms <- ggplot(all_monthly, aes(x=nforms, y=..count..)) + 
  geom_density(alpha=.2, colour="black", fill="cornflowerblue") + 
  scale_x_continuous(limits=c(0,150)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=6), 
        axis.title=element_text(size=6)) + 
  geom_vline(xintercept = summary_stats["median", "nforms"], linetype = "dashed") + 
  xlab("# forms")

g_nvisits <- ggplot(all_monthly, aes(x=nvisits, y=..count..)) + 
  geom_density(alpha=.2, colour="black", fill="cornflowerblue") + 
  scale_x_continuous(limits=c(0,100)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=6), 
        axis.title=element_text(size=6)) + 
  geom_vline(xintercept = summary_stats["median", "nvisits"], linetype = "dashed") + 
  xlab("# visits")

g_cases <- ggplot(all_monthly, aes(x=ncases_touched, y=..count..)) + 
  geom_density(alpha=.2, colour="black", fill="cornflowerblue") + 
  scale_x_continuous(limits=c(0,70)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=6), 
        axis.title=element_text(size=6)) + 
  geom_vline(xintercept = summary_stats["median", "ncases_touched"], linetype = "dashed") + 
  xlab("# cases")

g_cases_reg <- ggplot(all_monthly, aes(x=ncases_registered, y=..count..)) + 
  geom_density(alpha=.2, colour="black", fill="cornflowerblue") + 
  scale_x_continuous(limits=c(0,30)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=6), 
        axis.title=element_text(size=6)) + 
  geom_vline(xintercept = summary_stats["median", "ncases_registered"], linetype = "dashed") + 
  xlab("# cases registered")

g_cases_fu <- ggplot(all_monthly, aes(x=nunique_followups, y=..count..)) + 
  geom_density(alpha=.2, colour="black", fill="cornflowerblue") + 
  scale_x_continuous(limits=c(0,60)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=6), 
        axis.title=element_text(size=6)) +  
  geom_vline(xintercept = summary_stats["median", "nunique_followups"], linetype = "dashed") + 
  xlab("# cases followed-up")

g_per_active <- ggplot(all_monthly, aes(x=active_day_percent, y=..count..)) + 
  geom_density(alpha=.2, colour="black", fill="cornflowerblue") + 
  scale_x_continuous(limits=c(0,100)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=6), 
        axis.title=element_text(size=6)) + 
  geom_vline(xintercept = summary_stats["median", "active_day_percent"], linetype = "dashed") + 
  xlab("% of active days")

g_time_cc <- ggplot(all_monthly, aes(x=time_using_cc, y=..count..)) + 
  geom_density(alpha=.2, colour="black", fill="cornflowerblue") + 
  scale_x_continuous(limits=c(0,800)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=6), 
        axis.title=element_text(size=6)) + 
  geom_vline(xintercept = summary_stats["median", "time_using_cc"], linetype = "dashed") + 
  xlab("Total duration of CommCare use (minutes)")

pdf("histogram.pdf")
grid.arrange(g_nforms, g_nvisits, g_cases, g_cases_reg, g_cases_fu, 
             g_per_active, g_time_cc, nrow = 3, ncol = 3) 
dev.off()

#------------------------------------------------------------------------#
#Figure 4: Stacked bar graph of programs by sectors and subsectors
#Collapse sectors for the top 6 + other
domain$sector_nsf <- domain$sector_final
domain$sector_nsf[domain$sector_final == "Agriculture and Food Security"] <- "Agriculture" 
domain$sector_nsf[domain$sector_final == "Financial Services for the Poor"] <- "Economic Development"
domain$sector_nsf[domain$sector_final == "Education and Training"] <- "Education"
domain$sector_nsf[domain$sector_final == "Gender Equality and Women's Empowerment"] <- "Gender Equality"
domain$sector_nsf[domain$sector_final == "Gender_Equality"] <- "Gender Equality"
domain$sector_nsf[domain$sector_final == "Natural Resource Management"] <- "Other"
domain$sector_nsf[domain$sector_final == "Emergency Response"] <- "Other"
domain$sector_nsf[domain$sector_final == "Water, Sanitation, & Hygiene"] <- "Other"
domain$sector_nsf[domain$sector_final == "Governance and Accountability"] <- "Other"

domain_stacked <- filter(domain, !is.na(sector_nsf))
domain_sector_totals <- domain_stacked %>% group_by(sector_nsf) %>% 
  summarise(ndomains = length(unique(domain_id)))
domain_sector_totals <- arrange(domain_sector_totals, desc(ndomains))

ggplot(domain_sector_totals, aes(" ", ndomains, fill=sector_nsf)) + 
  geom_bar(stat="identity") + 
  scale_fill_brewer(palette = "Set3") +
  theme_bw() + 
  labs(x="", fill = "Program sector") + 
  ylab("Number of programs") + 
  scale_x_discrete(breaks=NULL) + 
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=10)) + 
  theme(legend.title=element_text(size=10), 
        legend.position = "left") + 
  theme(legend.text=element_text(size=10))

#Collapse subsectors for the top 6 + other
domain$subsector_nsf <- domain$subsector_final
domain_subsector_stacked <- filter(domain, !is.na(subsector_nsf))
domain_subsector_stacked <- filter(domain_subsector_stacked, sector_final == "Health")
domain_subsector_stacked$subsector_nsf[domain_subsector_stacked$subsector_final == "Cardiac Diseases"] <- "Other" 
domain_subsector_stacked$subsector_nsf[domain_subsector_stacked$subsector_final == "Diagnostics"] <- "Other" 
domain_subsector_stacked$subsector_nsf[domain_subsector_stacked$subsector_final == "Diarrhea"] <- "Other" 
domain_subsector_stacked$subsector_nsf[domain_subsector_stacked$subsector_final == "Family Planning"] <- "Other" 
domain_subsector_stacked$subsector_nsf[domain_subsector_stacked$subsector_final == "Infectious Diseases"] <- "Other" 
domain_subsector_stacked$subsector_nsf[domain_subsector_stacked$subsector_final == "Informatics"] <- "Other" 
domain_subsector_stacked$subsector_nsf[domain_subsector_stacked$subsector_final == "Malaria"] <- "Other" 
domain_subsector_stacked$subsector_nsf[domain_subsector_stacked$subsector_final == "Medication adherence"] <- "Other" 
domain_subsector_stacked$subsector_nsf[domain_subsector_stacked$subsector_final == "Mental Health"] <- "Other" 
domain_subsector_stacked$subsector_nsf[domain_subsector_stacked$subsector_final == "Neglected Tropical Diseases"] <- "Other" 
domain_subsector_stacked$subsector_nsf[domain_subsector_stacked$subsector_final == "Polio"] <- "Other" 
domain_subsector_stacked$subsector_nsf[domain_subsector_stacked$subsector_final == "Primary Care"] <- "Other" 
domain_subsector_stacked$subsector_nsf[domain_subsector_stacked$subsector_final == "Respiratory Diseases"] <- "Other" 
domain_subsector_stacked$subsector_nsf[domain_subsector_stacked$subsector_final == "Vaccinations"] <- "Other"

domain_subsector_totals <- domain_subsector_stacked %>% group_by(subsector_nsf) %>% 
  summarise(ndomains = length(unique(domain_id)))
domain_subsector_totals <- arrange(domain_subsector_totals, desc(ndomains))

p <- ggplot(domain_subsector_totals, aes(" ", ndomains, fill=subsector_nsf)) + 
  geom_bar(stat="identity") + 
  scale_fill_brewer(palette = "RdGy") +
  theme_bw() + 
  labs(x="", fill = "Health program subsector") + 
  ylab("Number of programs") + 
  scale_x_discrete(breaks=NULL) + 
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=10)) + 
  theme(legend.title=element_text(size=10)) + 
  theme(legend.text=element_text(size=10))
