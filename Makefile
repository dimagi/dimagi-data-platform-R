# TODO: One thing I haven't figured out smoothly is how to model
# dependencies between source files. Specifically, visit_table_run.R
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
RAW_DATA_DIR = ~/Dropbox/dimagi-data-platform-R/my_test_data
DBNAME = test

# The leading dash prevents make from exiting on an error.
DBSTAMP = .database.stamp
$(DBSTAMP):
	-createdb $(DBNAME)
	touch $(DBSTAMP)

# For each table in the database we have a timestamped file:
# http://www.postgresql.org/message-id/5071.1074453027@sss.pgh.pa.us
INTERACTIONS_CSV = $(RAW_DATA_DIR)/interactions.csv
VISITS_R = aggregate_tables/visit_table_run.R
VISITS_TABLE = .visits_table.stamp

$(VISITS_TABLE): $(VISITS_R) $(INTERACTIONS_CSV) $(DBSTAMP)
	Rscript $(VISITS_R) $(INTERACTIONS_CSV) $(DBNAME)
	touch $(VISITS_TABLE)

INDICATORS_TABLES = .indicators_tables.stamp
INDICATORS_R = indicators.R
INDICATORS_JSON = indicators.json

$(INDICATORS_TABLES): $(INDICATORS_R) $(VISITS_TABLE) $(INDICATORS_JSON) indicator_functions.R
	Rscript -e "source('$(INDICATORS_R)')" -e "write_tables('$(INDICATORS_JSON)')"
	touch $(INDICATORS_TABLES)

indicators: $(INDICATORS_TABLES)

# TODO: It would be good to find a smooth way to run tests. Maybe make
# this into an R package.
test:
	R -e "library(testthat)" -e "test_dir('tests')"
