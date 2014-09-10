INTERACTIONS_R = aggregate_tables/interaction_table_run.R
INTERACTIONS_TABLE = .interactions_table.stamp
INTERACTIONS_TABLE_NAME = interactions

$(INTERACTIONS_TABLE): $(INTERACTIONS_R) function_libraries/db_queries.R
	Rscript -e "source('$(INTERACTIONS_R)')" -e "write_visits('$(INTERACTIONS_TABLE_NAME)')"
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
