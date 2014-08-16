library(dplyr)
library(DBI)
library(rjson)

source('s_dplyr.R')
source('indicator_functions.R')

aggregate_table <- function(data, indicator.names) {
    f <- function(block) {
        vector <- sapply(indicator.names, function(iname) get(iname)(block))
        df <- as.data.frame(t(vector))
        names(df) <- indicator.names
        return(df)
    }
    return(data %.% do(f(.)))
}

aggregate_tables <- function() {
    indicator.info <- fromJSON(file='indicators.json')[[indicator.type]]
    indicator.names <- indicator.info[['functions']]
    group.by.str <- paste(indicator.info[['by']], collapse=', ')

    indicators <- visits %.% s_group_by(group.by.str) %.%
        aggregate_table(indicator.names)
}

rewrite_tables <- function(dbname) {
    db <- src_postgres(dbname=dbname)
    visits <- tbl(db, 'visits')

    dbRemoveTable(db$con, name=indicator.type)
    copy_to(db, df=indicators, name=indicator.type, temporary=FALSE)
}

main <- function() {
    args <- commandArgs(trailingOnly=TRUE)
    indicator.type <- args[1]
    dbname <- args[2]

    # make_indicators(dbname, indicator.type)
}
