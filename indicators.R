library(dplyr)
library(DBI)

## I've ported the first few indicators from lifetime_run.R. There are
## a lot more indicators to port over...

nvisits <- function(x) nrow(x)
first_visit_date <- function(x) min(x$visit_date)
last_visit_date <- function(x) max(x$visit_date)
days_on_cc <- function(x) as.numeric(last_visit_date(x) - first_visit_date(x)) + 1

lifetime.indicators <- list(
    'nvisits',
    'first_visit_date',
    'last_visit_date',
    'days_on_cc'
    )

monthly.indicators <- list(
    'nvisits',
    'days_on_cc'
    )

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

## TODO: Combine MakeLifetimeIndicators and MakeMonthlyIndicators.
MakeLifetimeIndicators <- function(dbname) {
    db <- src_postgres(dbname=dbname)
    visits <- tbl(db, 'visits')

    lifetime <- group_by(visits, user_id) %.%
        Aggregate(lifetime.indicators)
    MyCopy(db, lifetime, 'lifetime')
}

MakeMonthlyIndicators <- function(dbname) {
    db <- src_postgres(dbname=dbname)
    visits <- tbl(db, 'visits')

    monthly <- group_by(visits, month.index, user_id) %.%
        Aggregate(lifetime.indicators)
    MyCopy(db, monthly, 'monthly')
}

args <- commandArgs(trailingOnly=TRUE)
indicator.type <- args[1]
dbname <- args[2]

if (indicator.type == 'lifetime') {
    MakeLifetimeIndicators(dbname)
} else if (indicator.type == 'monthly') {
    MakeMonthlyIndicators(dbname)
}
