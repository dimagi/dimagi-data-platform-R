# dimagi-data-platform-R

This repository has two main components:

1.  Indicator framework for quickly generating aggregate indicators.
2.  Reporting framework for generating reports from those indicators.

## Indicator framework

To build the monthly and lifetime indicator tables issue the following
command:

    make indicators

This calls [indicators.R](./blob/master/indicators.R), which creates
tables described in [indicators.json](./blob/master/indicators.json)
and [indicator_functions.R](./blob/master/indicator_functions.R).

How does one add an indicator?
[indicators.json](./blob/master/indicators.json) contains a list of
tables to be created. Each element in the list is a dictionary with
four pairs:

1.  The name of the database where the table will be made.
2.  The name of the table to be made.
3.  The variables to group the aggregation by. For instance, in the
    monthly table aggregation is grouped by domain, user, and month.
4.  A list of components that will be merged together to build the
    final table.

Each component has three key-value pairs:

1.  The name of the database where the data for this component is
    pulled.
2.  The name of the table where the data for this component is pulled.
3.  The columns to be created.

Here is an example configuration:

    [
        {
            "database": "dimagi_data_platform",
            "table": "monthly",
            "by": ["domain", "user_id", "month.index"],
            "components": [
                {
                    "database": "dimagi_data_platform",
                    "table": "interactions",
                    "columns": {
                        "column1": "date_first_visit",
                        "column2": "date_last_visit"
                    }
                },
                {
                    "database": "dimagi_data_platform",
                    "table": "device_logs",
                    "columns": {
                        "column3": "nrow"
                    }
                }
            ]
        }
    ]

This will create a new table called `monthly` in the
`dimagi_data_platform` with three columns. `column1` will be the date
of the first visit a user performed in the month. `column2` will be
the date of the last visit a user performed in the month. `column3`
will count the number of device logs recorded in a month for a given
user. Notice that the key for each column is the name of the column
where that indicator will be stored, the value for each column is the
R function to be called on each block of data. To modularize the code
a bit, we have moved custom indicator functions into
[indicator_functions.R](./blob/master/indicator_functions.R). Here is
the definition of `date_first_visit`:

    date_first_visit <- function(x) min(x$visit_date, na.rm=TRUE)

This function takes a data.frame `x` and returns the minimum value of
the `visit_date` column of `x`. So, adding an indicator always
requires modifying [indicators.json](./blob/master/indicators.json)
and if the necessary R function to calculate the indicator does not
exist then it must be added to
[indicator_functions.R](./blob/master/indicator_functions.R).

## Reporting framework
