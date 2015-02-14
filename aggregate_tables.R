library(dplyr)
library(DBI)
library(rjson)

source('s_dplyr.R')
source('aggregate_tables/indicator_functions.R')
source('data_sources.R')

# TODO this shouldn't be both here and in config_file_funcs, but don't want to load jsonlite here
get_db_connection <- function(system_config_path='config_system.json') {
    config <- fromJSON(file=system_config_path)
    db_config <- config[['data_platform']][['database']]
    if ('pass' %in% names(db_config)) {
        names(db_config)[names(db_config) == 'pass'] <- 'password'
    }
    db <- do.call(src_postgres, db_config)
    return(db)
}

drop_tables <- function(file) {
  config <- fromJSON(file=file)
  
  for (table.info in config) {
    print(paste('Dropping', table.info$table, 'indicator table.'))
    db <- get_db_connection()
    dbRemoveTable(db$con, name=table.info$table)
  }
}

write_tables <- function(file, debug) {
    config <- fromJSON(file=file)
    db <- get_db_connection()
    for (table.info in config) {
        print(paste('Computing indicators for ', table.info$table, 'indicator table.'))
        df <- compute_indicators(table.info, db, debug)
        print(paste('Writing ', table.info$table, 'indicator table.'))
        dbRemoveTable(db$con, name=table.info$table)
        copy_to(db, df=df, name=table.info$table, temporary=FALSE)
    }
}

compute_indicators <- function(info, db, debug) {
  debug <- as.logical(debug)
  if (debug == T) {limit = 5000} else {limit = -1}
    dfs <- lapply(info$components, function(component) {
        print(paste('Getting data source ', component$table))
        source.data <- get_data_source(db, component$table, limit)
        group.by.str <- paste(info$by, collapse=', ')
        print(paste('Grouping and aggregating', component$table))
        df <- source.data %.% s_group_by(group.by.str) %.% aggregate(component$columns)
        return(df)
    })
    print('merging...')
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
