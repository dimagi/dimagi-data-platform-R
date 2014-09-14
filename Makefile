AGGREGATE_TABLES = .aggregate_tables.stamp
AGGREGATE_TABLES_R = aggregate_tables.R
AGGREGATE_TABLES_JSON = aggregate_tables.json
DEBUG_MODE = false

debug: DEBUG_MODE = true
debug: aggregate_tables

$(AGGREGATE_TABLES):
	Rscript -e "source('$(AGGREGATE_TABLES_R)')" -e "write_tables('$(AGGREGATE_TABLES_JSON)','$(DEBUG_MODE)')"
	touch $(AGGREGATE_TABLES)

aggregate_tables: $(AGGREGATE_TABLES)

test:
	R -e "library(testthat)" -e "test_dir('tests')"

clean:
	rm $(AGGREGATE_TABLES)
