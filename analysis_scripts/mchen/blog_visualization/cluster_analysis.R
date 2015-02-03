# cluster analyis
# get the kmeans 

# add scripts to compute kmeans here

# results: 4 clusters: x = 20, 40, 95, 270

# removing all x = y = 1
test3 <- test2[-which(test2$x == 1 & test2$y == 1),]
ca_sub1 <- test3[which(test3$x <= 30 & test3$y <= 30),]

crs <- test3[which(test3$domain == "crs-remind"),]
crs_sub <- crs[which(crs$x >= 10 & crs$x <= 40),]
smoothScatter(crs_sub$x, crs_sub$y)  

koraro <- test3[which(test3$domain == "mvp-koraro"),]
koraro_sub <- koraro[which(koraro$x >= 40 & koraro$x <= 100),]
smoothScatter(koraro_sub$x, koraro_sub$y)  

intensityPlotOut(crs_sub, koraro_sub, 100, 100)