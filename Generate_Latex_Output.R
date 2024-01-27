source("C:/Users/gilbe/Desktop/Master Thesis/R/Modelling.R")
source("C:/Users/gilbe/Desktop/Master Thesis/R/Robustness_checks.R")

#by continent
global_to_global_agg <- generate_agg_variable_timeseries(global_model_data%>%mutate(orig_continent = "Global"))
global_to_europe_agg <- generate_agg_variable_timeseries(europe_model_data%>%mutate(orig_continent = "Global"))

eu25_agg<-generate_agg_variable_timeseries(eu25_model_data%>%mutate(orig_continent="EU25"))
america_agg<-generate_agg_variable_timeseries(americas_model_data)
africa_agg<-generate_agg_variable_timeseries(africa_model_data)
asia_agg<-generate_agg_variable_timeseries(asia_model_data)

#by income
high_income_agg <- generate_agg_variable_timeseries(high_income_model_data%>%mutate(orig_continent = "High income"))
low_income_agg <- generate_agg_variable_timeseries(low_income_model_data%>%mutate(orig_continent = "Low income"))
low_middle_income_agg<-generate_agg_variable_timeseries(lowerm_income_model_data%>%mutate(orig_continent = "Lower middle"))
upper_middle_income_agg<-generate_agg_variable_timeseries(upperm_income_model_data%>%mutate(orig_continent = "Upper middle"))

#gen files by region
sink("C:/Users/gilbe/Desktop/Master Thesis/R/latex_regression_tables_global.txt")
generate_multicol_latex_table(list(model_global_to_global,model_global_to_europe,model_global_south_to_eu27,model_global_north_to_eu27))
sink("C:/Users/gilbe/Desktop/Master Thesis/R/latex_regression_tables_continents.txt")
generate_multicol_latex_table(list(model_europe_to_europe,model_asia_to_europe,model_africa_to_europe,model_americas_to_europe))

#gen files by north south
sink("C:/Users/gilbe/Desktop/Master Thesis/R/latex_regression_tables_northsouth.txt")
generate_multicol_latex_table(list(model_global_south_to_eu27,model_global_north_to_eu27))
#sink("C:/Users/gilbe/Desktop/Master Thesis/R/latex_regression_tables_conditioned.txt")
#generate_multicol_latex_table(list(agrimodel_low_income_to_europe,agrimodel_high_income_to_europe,agrimodel_upperm_income_to_europe,agrimodel_lowerm_income_to_europe))

#robcheck by region
sink("C:/Users/gilbe/Desktop/Master Thesis/R/latex_regression_tables_continent_robcheck_visa.txt")
generate_multicol_latex_table_robcheck(list(rob_global,rob_europe,rob_eu25,rob_asia,rob_africa,rob_americas))
sink("C:/Users/gilbe/Desktop/Master Thesis/R/latex_regression_tables_continent_robcheck_novisa.txt")
generate_multicol_latex_table(list(rob_global_novisa,rob_europe_novisa,rob_eu25_novisa,rob_asia_novisa,rob_africa_novisa,rob_americas_novisa))

#robcheck by income
sink("C:/Users/gilbe/Desktop/Master Thesis/R/latex_regression_tables_income_robcheck_visa.txt")
generate_multicol_latex_table_robcheck(list(rob_high,rob_low,rob_upperm,rob_lowerm))
sink("C:/Users/gilbe/Desktop/Master Thesis/R/latex_regression_tables_income_robcheck_novisa.txt")
generate_multicol_latex_table(list(rob_high_novisa,rob_low_novisa,rob_upperm_novisa,rob_lowerm_novisa))

#robcheck return migration
#sink("C:/Users/gilbe/Desktop/Master Thesis/R/latex_regression_tables_returnm_robcheck.txt")
#generate_multicol_latex_table_robcheck(list(model_global_to_europe_robcheck_mrate,model_asia_to_europe_robcheck_mrate,
                                            #model_africa_to_europe_robcheck_mrate,model_americas_to_europe_robcheck_mrate))


sink("C:/Users/gilbe/Desktop/Master Thesis/R/latex_agg_tables_per_continent.txt")
xtable(global_to_global_agg, caption = "Global migration dataset: averaged model variables over time",include.rownames = FALSE)
xtable(global_to_europe_agg, caption = "Global to Europe migration subset: averaged model variables over time",include.rownames = FALSE)
xtable(eu25_agg, caption = "EU27 to EU27 migration subset: averaged model variables over time",include.rownames = FALSE)
xtable(america_agg, caption = "America to EU27 migration subset: averaged model variables over time",include.rownames = FALSE)
xtable(africa_agg, caption = "Africa to Europe migration subset: averaged model variables over time",include.rownames = FALSE)
xtable(asia_agg, caption = "Asia to Europe migration subset: averaged model variables over time",include.rownames = FALSE)

sink("C:/Users/gilbe/Desktop/Master Thesis/R/latex_agg_tables_per_incomelevel.txt")
xtable(high_income_agg, caption = "High income countries to EU27: averaged model variables over time",include.rownames = FALSE)
xtable(low_income_agg, caption = "Low income countries to EU27: averaged model variables over time",include.rownames = FALSE)
xtable(low_middle_income_agg, caption = "Lower middle income countries to EU27: averaged model variables over time",include.rownames = FALSE)
xtable(upper_middle_income_agg, caption = "Upper middle income countries to EU27: averaged model variables over time",include.rownames = FALSE)

closeAllConnections()

