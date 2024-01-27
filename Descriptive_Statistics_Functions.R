

print_sample_statistics <- function(column){
  print(paste("Mean: ",mean(column)))
  print(paste("Sd: " ,sd(column)))
  print(paste("Med.: ",median(column)))
  print(paste("Min.: ",min(column)))
  print(paste("Max.: ",max(column)))
}

return_zero_share <- function(df, year){
  df <- df%>%filter(year0 == year)%>%select(mig_flow_sd)
  return(sum(df == 0)/nrow(df))
}

plot_map_europe_zero_flows <- function(df, group_by_column, continent_column, year_flow) {
  
  zero_flows <- df %>%
    group_by({{group_by_column}}) %>%
    filter({{continent_column}} == "Europe", year0 == year_flow) %>%
    summarise(zero_share = sum(mig_flow_sd == 0)/n()*100)
  
  SHP_27 <- get_eurostat_geospatial(resolution = 10, 
                                    nuts_level = 0, 
                                    year = 2021) %>% 
    select(geo = NUTS_ID, geometry) %>% 
    arrange(geo) %>% 
    st_as_sf()
  
  # Create a temporary column with country codes and rename it later
  SHP_27 <- SHP_27 %>%
    select(everything()) %>%
    mutate(temp_column = countrycode(geo, "iso2c", "iso3c"))
  
  SHP_27$temp_column <- ifelse(SHP_27$geo == "UK","GBR",SHP_27$temp_column)
  SHP_27$temp_column <- ifelse(SHP_27$geo =="EL","GRC",SHP_27$temp_column)         
  
  # Rename the temporary column to the desired column name
  names(SHP_27)[names(SHP_27) == "temp_column"] <- quo_name(enquo(group_by_column))
  
  zero_flows <- zero_flows %>% 
    select({{group_by_column}}, zero_share) %>% 
    inner_join(SHP_27, by = quo_name(enquo(group_by_column))) %>% 
    st_as_sf()
  
  interval_start <- year_flow
  interval_end <- year_flow + 4
  
  zero_flows %>% 
    ggplot(aes(fill = zero_share)) +
    geom_sf() +
    scale_x_continuous(limits = c(-10, 35),label=NULL) +
    scale_y_continuous(limits = c(35, 65),label=NULL) +
    scale_size(limits=c(0,1))+
    labs(title = paste("Share of zero migration flows in Europe from ",interval_start,"to",interval_end), fill = "zero flow share")+
    scale_fill_continuous(labels = label_number_si(),breaks=seq(0,100,by=25))+
    theme_void()+
    labs(fill = "Zero share in %")+
    theme(
      legend.key.height = unit(0.3, "cm"),  
      legend.key.width = unit(0.3, "cm"),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 6), 
      plot.title = element_text(margin = margin(b = 15)),
      legend.position = "left"
    )
}


plot_map_europe_stock <- function(df, group_by_1, group_by_2, continent_column, year_stock) {
  
  zero_flows <- df %>%
    group_by({{group_by_1}},{{group_by_2}}) %>%
    filter({{continent_column}} == "Europe", year0 == year_stock) %>%
    summarise(cum_stock = sum(mstock)*1000000)
  
  SHP_27 <- get_eurostat_geospatial(resolution = 10, 
                                    nuts_level = 0, 
                                    year = 2021) %>% 
    select(geo = NUTS_ID, geometry) %>% 
    arrange(geo) %>% 
    st_as_sf()
  
  # Create a temporary column with country codes and rename it later
  
  SHP_27 <- SHP_27 %>%
    select(everything()) %>%
    mutate(temp_column = countrycode(geo, "iso2c", "iso3c"))
  
  SHP_27$temp_column <- ifelse(SHP_27$geo == "UK","GBR",SHP_27$temp_column)
  SHP_27$temp_column <- ifelse(SHP_27$geo =="EL","GRC",SHP_27$temp_column)         
  
  # Rename the temporary column to the desired column name
  names(SHP_27)[names(SHP_27) == "temp_column"] <- quo_name(enquo(group_by_1))
  
  zero_flows <- zero_flows %>% 
    select({{group_by_1}}, cum_stock) %>% 
    inner_join(SHP_27, by = quo_name(enquo(group_by_1))) %>% 
    st_as_sf()
  
  interval_start <- year_stock
  interval_end <- year_stock + 4
  
  zero_flows %>% 
    ggplot(aes(fill = cum_stock)) +
    geom_sf() +
    scale_x_continuous(limits = c(-10, 35)) +
    scale_y_continuous(limits = c(35, 65)) +
    scale_size(limits=c(0,1))+
    ggtitle(paste("Cum. Migration Stocks in Europe from ",interval_start,"to",interval_end))+
    scale_fill_continuous(labels = label_number_si(), limits = c(0,10000000),breaks = c(0,
                                                                                       2000000,
                                                                                       4000000,
                                                                                       6000000,
                                                                                       8000000,
                                                                                       10000000))+
    labs(fill = "Cum. stock")+
    theme_void()+
    theme(
      legend.key.height = unit(0.3, "cm"),  
      legend.key.width = unit(0.3, "cm"),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 6), 
      plot.title = element_text(margin = margin(b = 15)),
      legend.position = "left"
    )
}

plot_map_europe_flow <- function(df, group_by_1, group_by_2, continent_column, year_flow){
  
  zero_flows <- df %>%
    group_by({{group_by_1}},{{group_by_2}}) %>%
    filter({{continent_column}} == "Europe", year0 == year_flow) %>%
    summarise(cum_flow = (sum(mig_flow_sd)/first(avg_pop_d))*100)
  
  SHP_27 <- get_eurostat_geospatial(resolution = 10, 
                                    nuts_level = 0, 
                                    year = 2021) %>% 
    select(geo = NUTS_ID, geometry) %>% 
    arrange(geo) %>% 
    st_as_sf()
  
  # Create a temporary column with country codes and rename it later
  SHP_27 <- SHP_27 %>%
    select(everything()) %>%
    mutate(temp_column = countrycode(geo, "iso2c", "iso3c"))
  
  SHP_27$temp_column <- ifelse(SHP_27$geo == "UK","GBR",SHP_27$temp_column)
  SHP_27$temp_column <- ifelse(SHP_27$geo =="EL","GRC",SHP_27$temp_column)         
  
  # Rename the temporary column to the desired column name
  names(SHP_27)[names(SHP_27) == "temp_column"] <- quo_name(enquo(group_by_1))
  
  zero_flows <- zero_flows %>% 
    select({{group_by_1}}, cum_flow) %>% 
    inner_join(SHP_27, by = quo_name(enquo(group_by_1))) %>% 
    st_as_sf()
  
  interval_start <- year_flow
  interval_end <- year_flow + 4
  
  zero_flows %>% 
    ggplot(aes(fill = cum_flow)) +
    geom_sf() +
    scale_x_continuous(limits = c(-10, 35)) +
    scale_y_continuous(limits = c(35, 65)) +
    labs(title = paste("Cum. Migration Flows in Europe from ",interval_start,"to",interval_end), fill = "cum. flow")+
    scale_fill_continuous(labels = label_number_si(), limits = c(0,8),breaks = c(0,2,4,6,8))+
    labs(fill = "Cum. flow")+
    theme_void()+
    theme(
      legend.key.height = unit(0.3, "cm"),  
      legend.key.width = unit(0.3, "cm"),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 6), 
      plot.title = element_text(margin = margin(b = 15)),
      legend.position = "left"
    )
}

plot_global_map <- function(df1,df2,df3,df4,column,low,upp,legend_label,title){
  
  world_map <- map_data("world") %>% filter(! long > 180,region != "Antarctica")
  
  #https://stackoverflow.com/questions/30706124/plotting-the-world-map-in-r
  countries = world_map %>% 
    distinct(region) %>% 
    rowid_to_column()
  
  countries$ISO3 <- countrycode(countries$region, "country.name", "iso3c")
  countries1 <- merge(countries,df1,by.x ="ISO3",by.y="orig",all.x=TRUE )
  countries2 <- merge(countries,df2,by.x ="ISO3",by.y="orig",all.x=TRUE )
  countries3 <- merge(countries,df3,by.x ="ISO3",by.y="orig",all.x=TRUE )
  countries4 <- merge(countries,df4,by.x ="ISO3",by.y="orig",all.x=TRUE )
  
  map1<-countries1 %>% 
    ggplot(aes(fill = {{column}}, map_id = region)) +
    geom_map(map = world_map) +
    expand_limits(x = world_map$long, y = world_map$lat) +
    coord_map("moll") +
    theme_map()+
    labs(fill = legend_label)+
    scale_fill_continuous(limits = c(low,upp))+
    ggtitle(paste(title, "2000 to 2004"))+
    theme(
      legend.key.height = unit(0.3, "cm"),  # Adjust the height of the legend key
      legend.key.width = unit(0.3, "cm"),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 6) # Adjust the width of the legend key
    )
    
  map2<-countries2 %>% 
    ggplot(aes(fill = {{column}}, map_id = region)) +
    geom_map(map = world_map) +
    expand_limits(x = world_map$long, y = world_map$lat) +
    coord_map("moll") +
    theme_map()+
    labs(fill = legend_label)+
    scale_fill_continuous(limits = c(low,upp))+
    ggtitle(paste(title, "2005 to 2009"))+
    theme(
      legend.key.height = unit(0.3, "cm"),  # Adjust the height of the legend key
      legend.key.width = unit(0.3, "cm"),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 6) # Adjust the width of the legend key
    )
    
  map3<-countries3 %>% 
    ggplot(aes(fill = {{column}}, map_id = region)) +
    geom_map(map = world_map) +
    expand_limits(x = world_map$long, y = world_map$lat) +
    coord_map("moll") +
    theme_map()+
    labs(fill = legend_label)+
    scale_fill_continuous(limits = c(low,upp))+
    ggtitle(paste(title, "2010 to 2014"))+
    theme(
      legend.key.height = unit(0.3, "cm"),  # Adjust the height of the legend key
      legend.key.width = unit(0.3, "cm"),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 6) # Adjust the width of the legend key
    )
    
  map4<-countries4 %>% 
    ggplot(aes(fill = {{column}}, map_id = region)) +
    geom_map(map = world_map) +
    expand_limits(x = world_map$long, y = world_map$lat) +
    coord_map("moll") +
    theme_map()+
    labs(fill = legend_label)+
    scale_fill_continuous(limits = c(low,upp))+
    ggtitle(paste(title, "2015 to 2019"))+
    theme(
      legend.key.height = unit(0.3, "cm"),  # Adjust the height of the legend key
      legend.key.width = unit(0.3, "cm"),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 6) # Adjust the width of the legend key
    )
  
  return(grid.arrange(map1,
               map2,
               map3,
               map4,
               ncol = 2))
  
}


plot_global_map_gs <- function(df,column,low,upp,legend_label,title){
  
  world_map <- map_data("world") %>% filter(! long > 180,region != "Antarctica")
  
  #https://stackoverflow.com/questions/30706124/plotting-the-world-map-in-r
  countries = world_map %>% 
    distinct(region) %>% 
    rowid_to_column()
  
  countries$ISO3 <- countrycode(countries$region, "country.name", "iso3c")
  countries <- merge(countries,df,by.x ="ISO3",by.y="orig",all.x=TRUE )
  
  map<-countries %>% 
    ggplot(aes(fill = {{column}}, map_id = region)) +
    geom_map(map = world_map) +
    expand_limits(x = world_map$long, y = world_map$lat) +
    coord_map("moll") +
    theme_map()+
    labs(fill = legend_label)+
    scale_fill_continuous(limits = c(low,upp))+
    ggtitle(paste(title, "2000 to 2004"))+
    theme(
      legend.key.height = unit(0.3, "cm"),  # Adjust the height of the legend key
      legend.key.width = unit(0.3, "cm"),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 6) # Adjust the width of the legend key
    )
  return(map)
}


plot_timeseries_boxplot <- function(df, column, y_axis_label){
  
  df_new <- df[, c(column, "year0")]
  df_new$year0 <- ifelse(df_new$year0 == 2000, "2000-2004", ifelse(df_new$year0 == 2005, "2005-2009", ifelse(df_new$year0 == 2010, "2010-2014", "2014-2019")))
  df_new$year0 <- as.factor(df_new$year0)
  colnames(df_new)[colnames(df_new) == column] <- "value"
  
  ggplot(df_new) +
    geom_boxplot(aes(x = year0, y = value)) +
    labs(x = "Interval", y = y_axis_label) +
    theme_minimal()+
    theme(
      panel.background = element_rect(fill = "white"),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.text
    )
}

plot_mrate_scatter <- function(df,cut_0.05_mrate,variable_cont,variable_binary,y_label,legend_label,title){
  
  if (cut_0.05_mrate == TRUE){
    
    df <- df%>%filter(mrate <= 0.05)
  }
  
  ggplot(df, aes(mrate, {{variable_cont}}, fill = as.factor({{variable_binary}}),size = wage_diff)) +
    geom_point(shape = 21, size = 4, color = "NA", alpha = 0.7) +
    scale_fill_manual(name = legend_label,
                      label = c("Yes","No"),
                      values = c("0" = "grey", "1" = "#4682B4")) +
    scale_size_continuous(name = "Wage Difference", labels = scales::comma, breaks = pretty(df$wage_diff)) +
    theme_minimal() +
    ggtitle(title)+
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10)
    )+
    xlab("Bilateral migration rate")+
    ylab(y_label)
}

plot_mrate_scatter <- function(df, cut_0.05_mrate, variable_cont, variable_binary, y_label, legend_label, title){
  
  if (cut_0.05_mrate == TRUE){
    df <- df %>% filter(mrate <= 0.05)
  }
  
  high_cutoff <- quantile(df$wage_diff, probs = 0.8)  
  low_cutoff <- quantile(df$wage_diff, probs = 0.3)
  
  df$wage_diff_group <- cut(df$wage_diff, breaks = c(-Inf, low_cutoff, high_cutoff, Inf),
                            labels = c("Low", "Mid", "High"), include.lowest = TRUE)
  
  ggplot(df, aes(mrate, {{variable_cont}}, fill = as.factor({{variable_binary}}), size = wage_diff_group)) +
    geom_point(shape = 21, alpha = 0.7, color="darkgrey")+
    scale_fill_manual(name = legend_label,
                      label = c("Yes", "No"),
                      values = c("0" = "grey", "1" = "#4682B4")) +
    scale_size_manual(name = "Wage differential", labels = c("Low", "Mid", "High"),
                      values = c("Low" = 3, "Mid" = 6, "High" = 9)) +  
    theme_minimal() +
    ggtitle(title) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10)
    ) +
    xlab("Bilateral migration rate") +
    ylab(y_label)
}

