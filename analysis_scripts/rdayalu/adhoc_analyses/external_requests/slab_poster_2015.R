#Graph data from WHO:
#http://www.who.int/gho/publications/world_health_statistics/EN_WHS2013_Full.pdf
#Page 40

#Barplot for under-five mortality rate

year <- c(1990, 2011, 1990, 2011)
country <- c("top", "top", "bottom", "bottom")
under_five <- c(12, 5, 183, 112)
under_5_mortality <- data.frame(cbind(year, country, under_five))
under_5_mortality$country <- as.character(under_5_mortality$country)
under_5_mortality$under_five <- as.numeric(as.character(under_5_mortality$under_five))

ggplot(data=under_5_mortality, aes(x=year, y=under_five, fill=country)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_manual(values=c("darkmagenta", "darkseagreen3"), 
                    name="Country ranking",
                    breaks=c("bottom", "top"),
                    labels=c("Bottom 25%", "Top 25%")) + 
  xlab("") +
  ylab("Per 1,000 live births") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=34), 
        axis.title=element_text(size=34)) + 
  ggtitle("Under-five mortality rate") + 
  theme(plot.title = element_text(size=34, face="bold")) + 
  theme(legend.position=c(0.8, 0.8)) + 
  theme(legend.title=element_text(size=28), 
        legend.text=element_text(size=28))



#Barplot for maternal mortality ratio

year <- c(1990, 2010, 1990, 2010)
country <- c("top", "top", "bottom", "bottom")
maternal_mort <- c(13, 8, 927, 521)
mm_ratio <- data.frame(cbind(year, country, maternal_mort))
mm_ratio$country <- as.character(mm_ratio$country)
mm_ratio$maternal_mort <- as.numeric(as.character(mm_ratio$maternal_mort))

ggplot(data=mm_ratio, aes(x=year, y=maternal_mort, fill=country)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_manual(values=c("darkmagenta", "darkseagreen3"), 
                    name="Country ranking",
                    breaks=c("bottom", "top"),
                    labels=c("Bottom 25%", "Top 25%")) + 
  xlab("") +
  ylab("Per 100,000 live births") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size=34), 
        axis.title=element_text(size=34)) + 
  ggtitle("Maternal mortality ratio") + 
  theme(plot.title = element_text(size=34, face="bold")) + 
  theme(legend.position=c(0.8, 0.8)) + 
  theme(legend.title=element_text(size=28), 
        legend.text=element_text(size=28))