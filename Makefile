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
DBSTAMP = .database.stamp

# The leading dash prevents make from exiting on an error.
$(DBSTAMP):
	-createdb $(DBNAME)
	touch $(DBSTAMP)

# For each table in the database we have a timestamped file:
# http://www.postgresql.org/message-id/5071.1074453027@sss.pgh.pa.us
INTERACTIONS_CSV = $(RAW_DATA_DIR)/interactions.csv
VISITS_R = aggregate_tables/visit_table_run.R
VISITS_TABLE = .visits-table.stamp
DBNAME = test

$(VISITS_TABLE): $(VISITS_R) $(INTERACTIONS_CSV) $(DBSTAMP)
	Rscript $(VISITS_R) $(INTERACTIONS_CSV) $(DBNAME)
	touch $(VISITS_TABLE)
