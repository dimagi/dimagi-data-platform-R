# TODO: One thing I haven't figured out smoothly is how to model
# dependencies between source files. Specifically, interaction_table_run.R
# depends on life_time_func.R, it would be ideal to have this
# explicitly stated in our Makefile. Here are some notes on how others
# have solved this problem:
# http://stackoverflow.com/questions/3120853/makefile-efficient-way-to-make-all-c-files-depend-on-a-header-file-with-the

# To get this Makefile started I am going to put paths to files on my
# computer directly into the Makefile. Down the road it would be good
# to use environment variables to get these paths in here.  Make
# automatically imports environment variables as macros, so you can
# reference an environment variable such as PATH with the makefile
# expression $(PATH).
# http://www.opussoftware.com/tutorial/TutMakefile.htm
DBNAME = dimagi_data_platform

# The leading dash prevents make from exiting on an error.
#	createuser -s -r importer
#	createuser -s -r reader
#	createuser -s -r postgres
#	createdb dimagi_data_platform
#	pg_restore -d dimagi_data_platform db.dump 
DBSTAMP = .database.stamp
$(DBSTAMP):
	-createdb $(DBNAME)
	touch $(DBSTAMP)

# For each table in the database we have a timestamped file:
# http://www.postgresql.org/message-id/5071.1074453027@sss.pgh.pa.us
INTERACTIONS_R = aggregate_tables/interaction_table_run.R
INTERACTIONS_TABLE = .interactions_table.stamp
INTERACTIONS_TABLE_NAME = interactions

$(INTERACTIONS_TABLE): $(INTERACTIONS_R) function_libraries/db_queries.R $(DBSTAMP)
	Rscript $(INTERACTIONS_R) $(DBNAME) $(INTERACTIONS_TABLE_NAME)
	touch $(INTERACTIONS_TABLE)

INDICATORS_TABLES = .indicators_tables.stamp
INDICATORS_R = indicators.R
INDICATORS_JSON = indicators.json

$(INDICATORS_TABLES): $(INDICATORS_R) $(INTERACTIONS_TABLE) $(INDICATORS_JSON) indicator_functions.R
	Rscript -e "source('$(INDICATORS_R)')" -e "write_tables('$(INDICATORS_JSON)')"
	touch $(INDICATORS_TABLES)

indicators: $(INDICATORS_TABLES)

test:
	R -e "library(testthat)" -e "test_dir('tests')"
