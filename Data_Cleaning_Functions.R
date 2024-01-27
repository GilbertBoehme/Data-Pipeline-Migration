generate_stock_matrix <- function(df,year){
  
  stock_table <- df %>%
    filter(Year == year) %>%
    select(dest, orig, mstock)
  
  stock_table$dest <- factor(stock_table$dest)
  stock_matrix <- xtabs(mstock~dest+orig,stock_table)
  
  return(stock_matrix)
}

gen_agg_pop_data <- function(input_df,year_lower,year_upper){
  df <- input_df%>%filter( year_lower <= year0 & year0 < year_upper)%>%group_by(ISO3)%>%summarize(avg_pop_o = mean(na.omit(Pop_o)),avg_lifeexp_o = mean(na.omit(LifeExp_o)))
  df$year0 <- year_lower
  return(df)
} 

gen_agg_disaster_data <- function(input_df,year_lower,year_upper){
  df <- input_df%>%filter( year_lower <= Year & Year < year_upper)%>%group_by(ISO)%>%summarize(disaster_count = sum(na.omit(disaster_count)))
  df$year0 <- year_lower
  return(df)
} 

gen_agg_ltclimate_data <- function(input_df,year_lower,year_upper){
  df <- input_df%>%filter( year_lower <= Year & Year < year_upper)%>%group_by(ISO3)%>%summarize(ANN_5Y = mean(ANN), Mean_LT = first(mean),Sd_LT = first(sd))
  df$year0 <- year_lower
  return(df)
}

gen_agg_armed_conflict_data <- function(input_df,year_lower,year_upper){
  df <- input_df%>%filter( year_lower <= year & year < year_upper)%>%group_by(ISO3)%>%summarize(armed_conflicts_count = sum(na.omit(armed_conflicts_count)))
  df$year0 <- year_lower
  return(df)
} 

gen_agg_gdp_data <- function(input_df,year_lower,year_upper){
  df <- input_df%>%filter( year_lower <= Year & Year < year_upper)%>%group_by(ISO3)%>%summarize(avg_gdp_per_cap = mean(na.omit(gdp_per_cap)))
  df$year0 <- year_lower
  return(df)
} 
gen_agg_unemploy_data <- function(input_df,year_lower,year_upper){
  df <- input_df%>%filter( year_lower <= year & year < year_upper)%>%group_by(iso3c)%>%summarize(avg_unemploy = mean(na.omit(SL.UEM.TOTL.ZS)))
  df$year0 <- year_lower
  return(df)
} 

gen_agg_income_data <- function(input_df,year_lower,year_upper){
  df <- input_df%>%filter( year_lower <= Year & Year < year_upper)%>%group_by(ISO3)%>%summarize(avg_income_per_cap = mean(na.omit(income_per_cap)))
  df$year0 <- year_lower
  return(df)
}

gen_agg_visa_data <- function(input_df,year_lower,year_upper){
  df <- input_df%>%filter( year_lower <= Year & Year < year_upper)%>%group_by(orig,dest)%>%summarize(visa_required = first(na.omit(visa_required)))
  df$year0 <- year_lower
  return(df)
} 

gen_agg_agri_employ_data <- function(input_df,year_lower,year_upper){
  df <- input_df%>%filter( year_lower <= Year & Year < year_upper)%>%group_by(orig)%>%summarize(avg_agri_employ = first(na.omit(agri_employ_share)))
  df$year0 <- year_lower
  return(df)
} 

gen_agg_agri_land_data <- function(input_df,year_lower,year_upper){
  df <- input_df%>%filter( year_lower <= Year & Year < year_upper)%>%group_by(orig)%>%summarize(avg_agri_land = first(na.omit(agri_land_share)))
  df$year0 <- year_lower
  return(df)
} 
