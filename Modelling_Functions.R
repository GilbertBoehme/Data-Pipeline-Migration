
fit_feglm <- function(df){
  
  df$dest_year_interaction <- interaction(df$dest, df$year0, drop = TRUE)
  df$orig_dummy <- as.factor(df$orig)
  df$dest_dummy <- as.factor(df$dest)
  
  model <- feglm(mrate ~ log(dist)+ 
                   #avg_lifeexp_o+
                   log((1+mstock))+
                   log(wage_diff)+
                   temp_anom+
                   pre_anom+
                   disaster_count+
                   armed_conflicts_count+
                   comlang_ethno+ 
                   contig | dest_year_interaction+orig_dummy, data=df, family = quasipoisson("log"),vcov="HC1")
  
  return(model)
}

fit_feglm_conditioned <- function(df,condition){
  
  df$dest_year_interaction <- interaction(df$dest, df$year0, drop = TRUE)
  df$orig_dummy <- as.factor(df$orig)
  df$dest_dummy <- as.factor(df$dest)
  df$avg_agri_employ_class <- relevel(df$avg_agri_employ_class, ref = "Mid employ")
  

  formula_string <- paste(
    "mrate ~ log(dist) + log(1 + mstock) + log(wage_diff) + temp_anom * ", as.factor(condition),"+",
    "+ pre_anom*",as.factor(condition),
    "+ disaster_count + armed_conflicts_count + comlang_ethno + contig | dest_year_interaction + orig_dummy"
  )
  
  formula <- as.formula(formula_string)
  
  model <- feglm(
    formula,
    data = df,
    quasipoisson(link = "log"),
    vcov = "HC1"
  )
  
  return(model)
}

fit_feglm_conditioned2 <- function(df,condition1,condition2){
  
  df$dest_year_interaction <- interaction(df$dest, df$year0, drop = TRUE)
  df$orig_dummy <- as.factor(df$orig)
  df$dest_dummy <- as.factor(df$dest)
  
  formula_string <- paste(
    "mrate ~ dist + (1 + mstock) + wage_diff + temp_anom * ", as.factor(condition1),"*",as.factor(condition2),
    "+ pre_anom*",as.factor(condition1),"*",as.factor(condition2),
    "+ disaster_count + armed_conflicts_count + comlang_ethno + contig | dest_year_interaction + orig_dummy"
  )
  
  formula <- as.formula(formula_string)
  
  model <- feglm(
    formula,
    data = df,
    quasipoisson(link = "log"),
    vcov = "HC1"
  )
  
  return(model)
}

fit_feglm_robcheck_visa <- function(df){
  
  df$dest_year_interaction <- interaction(df$dest, df$year0, drop = TRUE)
  df$orig_dummy <- as.factor(df$orig)
  df$dest_dummy <- as.factor(df$dest)
  
  model <- feglm(mrate ~ dist+ 
                   #avg_lifeexp_o+
                   (1+mstock)+
                   wage_diff+
                   temp_anom+
                   pre_anom+
                   disaster_count+
                   armed_conflicts_count+
                   comlang_ethno+ 
                   contig+
                   visa_required| dest_year_interaction+orig_dummy, data=df, quasipoisson(link="log"),vcov="HC1")
  
  return(model)
}

fit_feglm_robcheck_mrate <- function(df){
  
  df$dest_year_interaction <- interaction(df$dest, df$year0, drop = TRUE)
  df$orig_dummy <- as.factor(df$orig)
  df$dest_dummy <- as.factor(df$dest)
  
  model <- feglm(mrate_robcheck ~ dist+ 
                   #avg_lifeexp_o+
                   (1+mstock)+
                   wage_diff+
                   temp_anom+
                   pre_anom+
                   disaster_count+
                   armed_conflicts_count+
                   comlang_ethno+ 
                   contig | dest_year_interaction+orig_dummy, data=df, quasipoisson(link="log"),vcov="HC1")
  
  return(model)
}

generate_multicol_latex_table <- function(list_of_models){
  
  print(etable(list_of_models, tex = TRUE,
         fitstat = c('n','cor2'),
         dict = c(mrate = "Bilateral migration rate",
                  dist = "Distance",
                  avg_lifeexp_o = "Life expectancy",
                  mstock = "Network",
                  wage_diff = "Wage differential",
                  temp_anom = "Temperature anomalies",
                  pre_anom = "Precipitation anomalies",
                  disaster_count = "Natural disasters",
                  armed_conflicts_count = "Armed violation",
                  comlang_ethno = "Shared language",
                  contig = "Shared border"),
         order = c("Distance","Network","Wage differential","Life expectancy","Armed violation",
                   "Shared language","Shared border","Temperature anomalies","Precipitation anomalies",
                   "Natural disasters"),
         digits = "r4",
         title = "TEST",
         fontsize = "small"
  ))
}

generate_multicol_latex_table_robcheck <- function(list_of_models){
  
  print(etable(list_of_models, tex = TRUE,
               fitstat = c('n','cor2'),
               dict = c(mrate = "Bilateral migration rate",
                        dist = "Distance",
                        avg_lifeexp_o = "Life expectancy",
                        mstock = "Network",
                        wage_diff = "Wage differential",
                        temp_anom = "Temperature anomalies",
                        pre_anom = "Precipitation anomalies",
                        disaster_count = "Natural disasters",
                        armed_conflicts_count = "Armed violation",
                        comlang_ethno = "Shared language",
                        contig = "Shared border",
                        visa = "Visa required"),
               order = c("Distance","Network","Visa required","Wage differential","Life expectancy","Armed violation",
                         "Shared language","Shared border","Temperature anomalies","Precipitation anomalies",
                         "Natural disasters"),
               digits = "r4",
               title = "TEST",
               fontsize = "small"
  ))
}

?etable

generate_agg_variable_timeseries <- function(df){
  
  df_new <- as.data.frame(bind_rows(
    df %>% 
      group_by(orig_continent, year0) %>% 
      summarize(avg_mrate = round(mean(mrate)*1000,2)) %>% 
      pivot_wider(names_from = year0, values_from = avg_mrate)%>%
      mutate(orig_continent = if_else(row_number() == 1, "Mirgation rate per k, %", orig_continent))
    ,
    df %>% 
      group_by(orig_continent, year0) %>% 
      summarize(avg_mstock = round(mean(mstock * 1000000),2)) %>% 
      pivot_wider(names_from = year0, values_from = avg_mstock)%>%
      mutate(orig_continent = if_else(row_number() == 1, "Network", orig_continent))
    ,
    df %>% 
      group_by(orig_continent, year0) %>% 
      summarize(avg_wdiff = round(mean(wage_diff),2)) %>% 
      pivot_wider(names_from = year0, values_from = avg_wdiff)%>%
      mutate(orig_continent = if_else(row_number() == 1, "Wage diff", orig_continent))
    ,
    df %>% 
      group_by(orig_continent, year0) %>% 
      summarize(avg_vio = round(mean(armed_conflicts_count*1000),2)) %>% 
      pivot_wider(names_from = year0, values_from = avg_vio)%>%
      mutate(orig_continent = if_else(row_number() == 1, "Armed violation", orig_continent))
    ,
    df %>% 
      group_by(orig_continent, year0) %>% 
      summarize(avg_tempanom = round(mean(temp_anom),2)) %>% 
      pivot_wider(names_from = year0, values_from = avg_tempanom)%>%
      mutate(orig_continent = if_else(row_number() == 1, "Temperature anomaly", orig_continent))
    ,
    df %>% 
      group_by(orig_continent, year0) %>% 
      summarize(avg_preanom = round(mean(pre_anom),2)) %>% 
      pivot_wider(names_from = year0, values_from = avg_preanom)%>%
      mutate(orig_continent = if_else(row_number() == 1, "Precipitation anomaly", orig_continent))
    ,
    df %>% 
      group_by(orig_continent, year0) %>% 
      summarize(avg_disaster = round(mean(disaster_count),2)) %>% 
      pivot_wider(names_from = year0, values_from = avg_disaster) %>%
      mutate(orig_continent = if_else(row_number() == 1, "Natural disasters", orig_continent))
    )
  )
  df_new <- df_new %>% rename(`Sample variable` = orig_continent,`2000-2005` = `2000`,`2005-2010` = `2005`,`2010-2015` = `2010`,`2015-2020` = `2015`)
  return(df_new)
}

plot_timeseries_boxplot <- function(df,column, y_axis_label){
  
  df_new <- df%>%select({{column}},year0)
  df_new$year0 <- ifelse(df_new$year0 == 2000,"2000-2005",df_new$year0)
  df_new$year0 <- ifelse(df_new$year0 == 2005,"2005-2010",df_new$year0)
  df_new$year0 <- ifelse(df_new$year0 == 2010,"2010-2015",df_new$year0)
  df_new$year0 <- ifelse(df_new$year0 == 2015,"2015-2020",df_new$year0)
  
  ggplot(df_new) +
    geom_boxplot(aes(x = factor(year0), y = {{column}})) +
    labs(x = "Interval", y = y_axis_label) +
    theme_minimal() 
}
