#-----------------------------------------------------------------------------#

#PRINT PLOTS AND EXPORT TO PDF

#-----------------------------------------------------------------------------#
require(gridExtra)
report_output_dir <- file.path(output_dir, "reports")
dir.create(report_output_dir, showWarnings = FALSE)

outfile <- file.path(report_output_dir,"Users_visits_total.pdf")
pdf(outfile)
grid.arrange(p_users, g_stacked_visits, nrow=2)
dev.off()

outfile <- file.path(report_output_dir,"Number_visits.pdf")
pdf(outfile)
grid.arrange(g_visits_overall, g_visits_med_split, nrow=2)
dev.off()

outfile <- file.path(report_output_dir,"Users_visits_total.pdf")
pdf(outfile)
grid.arrange(p_users, g_stacked_visits, nrow=2)
dev.off()

outfile <- file.path(report_output_dir,"Registered_cases.pdf")
pdf(outfile)
grid.arrange(g_reg_med_overall, g_reg_med_split, nrow=2)
dev.off()

outfile <- file.path(report_output_dir,"Cases_followed_up.pdf")
pdf(outfile)
grid.arrange(g_case_fu_overall, g_case_fu_split, nrow=2)
dev.off()