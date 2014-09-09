library(dplyr)
library(DBI)
library(rjson)

source('s_dplyr.R')
source('indicator_functions.R')

write_tables <- function(file) {
    config <- fromJSON(file=file)

    for (table.info in config) {
        df <- compute_indicators(table.info)

        db <- src_postgres(dbname=table.info$database)
        dbRemoveTable(db$con, name=table.info$table)
        copy_to(db, df=df, name=table.info$table, temporary=FALSE)
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
    merged <- Reduce(function(...) merge(..., all.x=TRUE, all.y=TRUE, by=info$by), dfs)
    return(merged)
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
