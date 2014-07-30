#-----------------------------------------------------------------------------#
#PRINT PLOTS AND EXPORT TO PDF
#-----------------------------------------------------------------------------#
require(gridExtra)
report_output_dir <- file.path(output_dir, "reports")
dir.create(report_output_dir, showWarnings = FALSE)

outfile <- file.path(report_output_dir,"Number_users.pdf")
pdf(outfile)
grid.arrange(p_users, p_violin_users, nrow=2)
dev.off()

outfile <- file.path(report_output_dir,"Number_visits_total.pdf")
pdf(outfile)
grid.arrange(g_stacked_visits, nrow=1)
dev.off()

outfile <- file.path(report_output_dir,"Visits_median.pdf")
pdf(outfile)
grid.arrange(g_visits_overall, g_visits_med_split, nrow=2)
dev.off()

outfile <- file.path(report_output_dir,"Active_days_median.pdf")
pdf(outfile)
grid.arrange(g_active_days_overall, g_active_days_split, nrow=2)
dev.off()

outfile <- file.path(report_output_dir,"Visits_per_day_median.pdf")
pdf(outfile)
grid.arrange(g_visits_per_day_overall, g_visits_per_day_split, nrow=2)
dev.off()

outfile <- file.path(report_output_dir,"Cases_modified_median.pdf")
pdf(outfile)
grid.arrange(g_cases_mod_overall, g_cases_mod_split, nrow=2)
dev.off()

outfile <- file.path(report_output_dir,"Number_cases_reg_total.pdf")
pdf(outfile)
grid.arrange(g_case_reg_sum, nrow=1)
dev.off()

outfile <- file.path(report_output_dir,"Registered_cases.pdf")
pdf(outfile)
grid.arrange(g_reg_med_overall, g_reg_med_split, nrow=2)
dev.off()

outfile <- file.path(report_output_dir,"Cases_followed_up.pdf")
pdf(outfile)
grid.arrange(g_case_fu_overall, g_case_fu_split, nrow=2)
dev.off()