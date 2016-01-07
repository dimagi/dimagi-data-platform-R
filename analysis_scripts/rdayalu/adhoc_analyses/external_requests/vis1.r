#!/usr/bin/Rscript

####################################################################################
# code developed by Klurig Analytics (Dag Holmboe)
# Oct 18, 2014
# Please don't remove this section
####################################################################################

####################################################################################
rm(list=ls())
suppressMessages(library(dplyr))

################################################################
add.months <- function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]

################################################################
# main
if( length( commandArgs() ) == 7 ) {
  input_file  <- commandArgs()[6]
  output_file <- commandArgs()[7]
} else {
  cat("\nError: not correct parameters\n")
  print(getwd())
  cat("Usage: program <input file> <output file with csv extension>\n\n")
  quit()
}

if(0) {
DEBUG=F
if(DEBUG) {
  input_file  <- "debug_input.csv"
  output_file <- "debug_output.csv"
} else {
  input_file  <- "original/sample_monthly.csv"
  output_file <- "sample_monthly_output.csv"
} }

p1 <- read.csv(input_file, stringsAsFactors=T)
p1 <- tbl_df(p1)

# only keep columns that are interesting
p1 <- p1 %>% select(user_id, calendar_month, month_index, previous_month_active, next_month_active) 
  
p1$calendar_month   <- as.Date(as.character(p1$calendar_month), format="%m/%d/%y")

df <- p1

if(1) {
  df1 <- df
} else {
  # select 7 users for debugging
#  df1 <- df %>% filter(grepl( "EK8", user_id) | grepl( "W1E", user_id) | grepl( "HXO", user_id) |
#    grepl( "X3J", user_id) | grepl( "R36", user_id) | grepl( "cf72", user_id) | grepl( "1363", user_id) )
  df1 <- df %>% filter(grepl( "EK8", user_id) | grepl( "W1E", user_id) | grepl( "HXO", user_id))
} 

# 1. figure out if a user/month is 'new', 'continued' or 'restarted'
df1$status <- sapply(df1$previous_month_active, function(x) {
  if(is.na(x)) return("f.started")
  if(x) return("continued")
  return("g.restart")})
  
# 2. bin the user/month into bins
df1$bin <- sapply(df1$month_index, function(x)
         if(x == 1)                      {return("x.one") 
  } else if(x == 2  | x == 3)            {return("e.twothree")
  } else if(x == 4  | x == 5  | x == 6)  {return("d.fourtosix")
  } else if(x == 7  | x == 8  | x == 9)  {return("c.seventonine")
  } else if(x == 10 | x == 11 | x == 12) {return("b.tentotwelve")
  } else                                 {return("a.morethantwelve") } )

# 3. create 'active' data structure
df.active <- df1 %>% group_by(calendar_month, bin, status) %>% summarize(  monthly_count = n() )
df.active[df.active$status == "f.started",]$bin <- "f.started" 
df.active[df.active$status == "g.restart",]$bin <- "g.restart" 
df.active$status <- NULL
df.active <- df.active %>% group_by(calendar_month, bin) %>% summarize( monthly_count = sum(monthly_count) )
  
# 4. create data frame for 'dropped' users.  Note that we are placing the 'dropped month' one month ahead
df.drop <- df1 %>% filter(next_month_active == FALSE)
df.drop$calendar_month <- as.Date( sapply( df.drop$calendar_month, function(x) add.months(x,1)), origin="1970-01-01")
df.drop$month_index <- df.drop$month_index + 1
df.drop$bin <- sapply(df.drop$month_index, function(x)
         if(x == 1)                      {return("a.one.d") 
  } else if(x == 2)                      {return("k.two.d")
  } else if(x == 3 | x == 4 | x == 5| x == 6)  {return("l.threetosix.d")
  } else if(x == 7  | x == 8  | x == 9)  {return("m.seventonine.d")
  } else if(x == 10 | x == 11 | x == 12) {return("n.tentotwelve.d")
  } else                                 {return("o.morethantwelve.d") } )
df.drop <- df.drop %>% 
  select(calendar_month, bin) %>% group_by(calendar_month, bin) %>% summarize(  monthly_count = n() )

# 5.  create a data structure that includes all months from start to finish.  This data structure
#   will be used to join other data structures such that we have all months
max_month <- add.months(max(df1$calendar_month), 1)
all.months <- data.frame(calendar_month=seq(min(df1$calendar_month),max_month, by="1 month"))

# 6. combine all data structures
df.all <- rbind(df.active, df.drop)
df.total <- left_join(all.months, df.all)

# 7. reshape "bins" to a wide data structure with months on the horizontal line
library(reshape2)
dfc <- dcast(df.total, bin~calendar_month)
dfc <- subset(dfc, bin != 0)
dfc[is.na(dfc)] <- 0

# 8. transpose the data structure - easier to calculate values
n <- dfc$bin
dfc.y <- as.data.frame(t(dfc[,-1]))
names(dfc.y) <- n

# 9. sum values
#   Because we don't know how many, nor which, columns will be part of the data structure
#   for adding, we use "f.started" as an anchor.
nactive <- which(names(dfc.y) == "f.started")
dfc.y$h.total_gains <- dfc.y$f.started + dfc.y$g.restart
dfc.y$i.active <- apply( dfc.y[,c((nactive+1):1)], 1, sum)
dfc.y$j.total_drops <- apply( dfc.y[,c(  (nactive+2):(ncol(dfc.y)-2)  )], 1, sum)

# 10. transpose to original data frame, and also order the table alphabetically 
n <- row.names(dfc.y)
dfc.x <- as.data.frame(t(dfc.y))
names(dfc.x) <- n
dfc.x <- dfc.x[ order(row.names(dfc.x)),]

  
cat("input file:", input_file, " output file:", output_file, "\n")
write.csv(dfc.x, file=output_file, row.names=T)


