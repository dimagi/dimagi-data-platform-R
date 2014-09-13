INDICATORS_TABLES = .indicators_tables.stamp
INDICATORS_R = indicators.R
INDICATORS_JSON = indicators.json
DEBUG = true

$(INDICATORS_TABLES): $(INDICATORS_R) $(INDICATORS_JSON) indicator_functions.R
	Rscript -e "source('$(INDICATORS_R)')" -e "write_tables('$(INDICATORS_JSON)','$(DEBUG)')"
	touch $(INDICATORS_TABLES)

indicators: $(INDICATORS_TABLES)

test:
	R -e "library(testthat)" -e "test_dir('tests')"
