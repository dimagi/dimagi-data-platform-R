# dimagi-data-platform-R

This repository has two main components:

1.  Aggregate table framework for generating aggregate tables.
2.  Reporting framework for generating reports from those aggregate tables.

## Aggregate table framework

To build the aggregate tables issue the following
commands:

    make clean
    make aggregate_tables

Alternatively, make debug can be used to build aggregate tables from a subset of rows from each data source. This is useful for data sources that involve long-running queries. Specify debug mode as follows:

    make clean
    make debug
    
Both forms call [aggregate_tables.R](aggregate_tables.R), which creates
tables described in [aggregate_tables.json](aggregate_tables.json)
using the indicator calculation functions in [indicator_functions.R](indicator_functions.R).

How does one add an indicator?
[aggregate_tables.json](aggregate_tables.json) contains a list of
tables to be created. Each element in the list is a dictionary with
four pairs:

1.  The name of the database where the table will be made.
2.  The name of the table to be made.
3.  The variables to group the aggregation by. For instance, in the
    monthly table aggregation is grouped by domain, user, and month.
4.  A list of components that will be merged together to build the
    final table.

Each component has two key-value pairs:

1.  The name of the table where the data for this component is pulled.
2.  The columns to be created.

Here is an example configuration:

    [
        {
            "table": "aggregate_monthly_interactions",
            "by": ["domain", "user_id", "month.index"],
            "components": [
                {
                    "table": "interactions",
                    "columns": {
                        "column1": "date_first_visit",
                        "column2": "date_last_visit"
                    }
                },
                {
                    "table": "device_logs",
                    "columns": {
                        "column3": "nrow"
                    }
                }
            ]
        }
    ]

This will create a new table called `aggregate_monthly_interactions` with three columns. 
`column1` will be the date of the first visit a user performed in the month. 
`column2` will be the date of the last visit a user performed in the month. 
`column3` will count the number of device logs recorded in a month for a given user. 
Notice that the key for each column is the name of the column
where that indicator will be stored, the value for each column is the
R function to be called on each block of data. To modularize the code
a bit, we have moved custom indicator functions into
[indicator_functions.R](indicator_functions.R). Here is
the definition of `date_first_visit`:

    date_first_visit <- function(x) min(x$visit_date, na.rm=TRUE)

This function takes a data.frame `x` and returns the minimum value of
the `visit_date` column of `x`. So, adding an indicator always
requires modifying [aggregate_tables.json](aggregate_tables.json)
and if the necessary R function to calculate the indicator does not
exist then it must be added to
[indicator_functions.R](indicator_functions.R).

## Reporting framework
Report modules produce one or more PDF reports. Report modules for a run are specified in config_run.json, along with options like split-bys and report dates. 

test_report.R is an example report module.

The script reports.R reads the config file, sets up variables (including filtering domains and returning a list of domains to include in the report run) and runs the report modules specified in the config.  Each report module should return a list of file names (full paths) specifying the individual pdfs it has written to tmp_pdf_report_dir. Once all report modules have run, these are collated into a single pdf in reports.R, using http://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/.

Report modules should provide a render function that returns a list of pdf files (full paths) generated in the report module. The render function should be defined as follows:

    render <- function (db,domains_for_run,report_options,tmp_report_pdf_dir)
