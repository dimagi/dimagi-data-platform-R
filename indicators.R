library(dplyr)
library(DBI)
library(rjson)

source('s_dplyr.R')
source('indicator_functions.R')

write_tables <- function(file) {
    config <- fromJSON(file=file)

    for (table.info in config) {
        df <- compute_indicators(table.info)
        print(df)
    }
}

compute_indicators <- function(info) {
    dfs <- lapply(info$components, function(component) {
        db <- src_postgres(dbname=component['database'])
        source.data <- tbl(db, component$table)
        group.by.str <- paste(info$by, collapse=', ')
        df <- source.data %.% s_group_by(group.by.str) %.% aggregate(component$columns)
        return(df)
    })
    if (length(dfs) == 1) {
        return(dfs[[1]])
    }
}

aggregate <- function(data, columns) {
    column.names <- names(columns)

    f <- function(block) {
        vector <- sapply(column.names, function(cname) get(columns[[cname]])(block))
        df <- as.data.frame(t(vector))
        names(df) <- column.names
        return(df)
    }
    return(data %.% do(f(.)))
}

write_table <- function(table.name, table.info) {
    dbRemoveTable(db$con, name=indicator.type)
    copy_to(db, df=indicators, name=indicator.type, temporary=FALSE)
}
