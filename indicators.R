library(dplyr)
library(DBI)
library(rjson)

source('s_dplyr.R')

## I've ported the first few indicators from lifetime_run.R. There are
## a lot more indicators to port over...

nvisits <- function(x) nrow(x)
first_visit_date <- function(x) min(x$visit_date)
last_visit_date <- function(x) max(x$visit_date)
days_on_cc <- function(x) as.numeric(last_visit_date(x) - first_visit_date(x)) + 1

Aggregate <- function(data, indicator.names) {
    f <- function(block) {
        vector <- sapply(indicator.names, function(iname) get(iname)(block))
        df <- as.data.frame(t(vector))
        names(df) <- indicator.names
        return(df)
    }
    return(data %.% do(f(.)))
}

## TODO: Move this to a utilities file.
MyCopy <- function(db, df, name) {
    dbRemoveTable(db$con, name=name)
    copy_to(db, df=df, name=name, temporary=FALSE)
}

MakeIndicators <- function(dbname, indicator.type) {
    db <- src_postgres(dbname=dbname)
    visits <- tbl(db, 'visits')

    indicator.info <- fromJSON(file='indicators.json')[[indicator.type]]
    indicator.names <- indicator.info[['functions']]
    group.by.str <- paste(indicator.info[['by']], collapse=', ')

    indicators <- visits %.% s_group_by(group.by.str) %.%
        Aggregate(indicator.names)
    MyCopy(db, indicators, indicator.type)
}

args <- commandArgs(trailingOnly=TRUE)
indicator.type <- args[1]
dbname <- args[2]

MakeIndicators(dbname, indicator.type)
