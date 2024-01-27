############################ ISO3 CHECK ############################

#231 unique country codes
length(unique(dat_migflow$orig))
length(unique(dat_migflow$dest))

#224 unique country codes dat_em_climate_disasters
length(unique(dat_em_climate_disasters$ISO))
setdiff(unique(dat_migflow$orig),unique(dat_em_climate_disasters$ISO))

#243 unique country codes dat_gravity_1
length(unique(dat_dist$iso_o))
summary(dat_dist$contig)

#208 unique country codes for dat_temp
length(unique(dat_temp$ISO3))
summary(dat_temp)

#206 unique country codes for dat_pre
length(unique(dat_pre$ISO3))
summary(dat_pre)

############################ INTERSECTION OF DATASETS ############################

# left merge dat_em_climate_disasters and dat_armedc to dat_abel, assume NaN's to be zero values

# Global intersection of Abel and Datasets
iso3_codes_migflow <- unique(dat_migflow$orig)
length(intersect(iso3_codes_migflow,dat_mstock$orig))
length(intersect(iso3_codes_migflow,dat_dist$iso_o))
length(intersect(iso3_codes_migflow,dat_temp$ISO3))
length(intersect(iso3_codes_migflow,dat_pre$ISO3))
#length(intersect(iso3_codes_abel,dat_demo$Code))

# -> min intersection of 203 country codes of pre data with abel data
length(intersect(dat_pre$ISO3,dat_dist$iso_o))
length(intersect(dat_pre$ISO3,dat_temp$ISO3))
length(intersect(dat_pre$ISO3,dat_mstock$dest))

setdiff(unique(dat_pre$ISO3),unique(dat_dist$iso_o))
setdiff(unique(dat_pre$ISO3),unique(dat_mstock$orig))

# -> min intersection of 198 country codes of pre data with migration stock data

############################ ISO-FILTERING OF DATASETS ############################

ISO3_filtered <- intersect(dat_pre$ISO3,dat_migflow$orig)

dat_migflow <- dat_migflow%>%select(everything())%>%filter(orig %in% ISO3_filtered)
dat_mstock <- dat_mstock%>%select(everything())%>%filter(orig %in% ISO3_filtered)
dat_em_climate_disasters <- dat_em_climate_disasters%>%select(everything())%>%filter(ISO %in% ISO3_filtered)
dat_temp <- dat_temp%>%select(everything())%>%filter(ISO3 %in% ISO3_filtered)
dat_pre <- dat_pre%>%select(everything())%>%filter(ISO3 %in% ISO3_filtered)
dat_dist <- dat_dist%>%select(everything())%>%filter(iso_o %in% ISO3_filtered)
dat_armedc <- dat_armedc%>%select(everything())%>%filter(ISO3 %in% ISO3_filtered)

setdiff(unique(dat_migflow$orig),unique(dat_mstock$orig))
setdiff(unique(dat_migflow$orig),unique(dat_temp$ISO3))
setdiff(unique(dat_migflow$orig),unique(dat_pre$ISO3))
setdiff(unique(dat_migflow$orig),unique(dat_dist$iso_o))

############################ AGG DATASETS ############################

# ST - climate data
dat_em_climate_disasters_2000 <- gen_agg_disaster_data(dat_em_climate_disasters,2000,2005)
dat_em_climate_disasters_2005 <- gen_agg_disaster_data(dat_em_climate_disasters,2005,2010)
dat_em_climate_disasters_2010 <- gen_agg_disaster_data(dat_em_climate_disasters,2010,2015)
dat_em_climate_disasters_2015 <- gen_agg_disaster_data(dat_em_climate_disasters,2015,2020)
dat_em_climate_disasters_agg <- rbind(dat_em_climate_disasters_2000,dat_em_climate_disasters_2005,dat_em_climate_disasters_2010,dat_em_climate_disasters_2015)

# LT - climate data

# long term average 1901 - 2019
dat_temp_lt_mean <- dat_temp%>%filter(YEAR>=1901,YEAR<=1989)%>%group_by(ISO3)%>%summarize(mean=mean(ANN))
dat_temp_lt_sd <- dat_temp%>%filter(YEAR>=1901,YEAR<=1989)%>%group_by(ISO3)%>%summarize(sd=sd(ANN))

dat_pre_lt_mean <- dat_pre%>%filter(YEAR>=1901,YEAR<=1989)%>%group_by(ISO3)%>%summarize(mean=mean(ANN))
dat_pre_lt_sd <- dat_pre%>%filter(YEAR>=1901,YEAR<=1989)%>%group_by(ISO3)%>%summarize(sd=sd(ANN))

dat_temp <- merge(x = dat_temp, dat_temp_lt_mean, by = "ISO3")
dat_temp <- merge(x = dat_temp, dat_temp_lt_sd, by = "ISO3")

dat_pre <- merge(x = dat_pre, dat_pre_lt_mean, by = "ISO3")
dat_pre <- merge(x = dat_pre, dat_pre_lt_sd, by = "ISO3")

colnames(dat_temp)[2]<- "Year"
colnames(dat_pre)[2]<- "Year"

dat_temp_2000 <- gen_agg_ltclimate_data(dat_temp,2000,2005)
dat_temp_2005 <- gen_agg_ltclimate_data(dat_temp,2005,2010)
dat_temp_2010 <- gen_agg_ltclimate_data(dat_temp,2010,2015)
dat_temp_2015 <- gen_agg_ltclimate_data(dat_temp,2015,2020)
dat_temp_agg <- rbind(dat_temp_2000,dat_temp_2005,dat_temp_2010,dat_temp_2015)

# LT - Pre data

dat_pre_2000 <- gen_agg_ltclimate_data(dat_pre,2000,2005)
dat_pre_2005 <- gen_agg_ltclimate_data(dat_pre,2005,2010)
dat_pre_2010 <- gen_agg_ltclimate_data(dat_pre,2010,2015)
dat_pre_2015 <- gen_agg_ltclimate_data(dat_pre,2015,2020)
dat_pre_agg <- rbind(dat_pre_2000,dat_pre_2005,dat_pre_2010,dat_pre_2015)

# economic and cultural data

#colnames(dat_gravity)[3] <- "Year"

#dat_gravity_2000 <- gen_agg_eco_cult_data(dat_gravity,2000,2005)

#dat_gravity_2000%>%filter(iso3_d=="DEU")

#dat_gravity_2005 <- gen_agg_eco_cult_data(dat_gravity,2005,2010)
#dat_gravity_2010 <- gen_agg_eco_cult_data(dat_gravity,2010,2015)
#dat_gravity_2015 <- gen_agg_eco_cult_data(dat_gravity,2015,2020)
#dat_gravity_agg <- rbind(dat_gravity_2000,dat_gravity_2005,dat_gravity_2010,dat_gravity_2015)
#dat_gravity_agg%>%filter(iso3_d=="DEU")
#wage differnetial

dat_gdp_per_cap_2000 <- gen_agg_gdp_data(dat_gdp_per_cap,2000,2005)
dat_gdp_per_cap_2005 <- gen_agg_gdp_data(dat_gdp_per_cap,2005,2010)
dat_gdp_per_cap_2010 <- gen_agg_gdp_data(dat_gdp_per_cap,2010,2015)
dat_gdp_per_cap_2015 <- gen_agg_gdp_data(dat_gdp_per_cap,2015,2020)
dat_gdp_per_cap_agg <- rbind(dat_gdp_per_cap_2000,dat_gdp_per_cap_2005,dat_gdp_per_cap_2010,dat_gdp_per_cap_2015)

#dat_income_per_cap_2000 <- gen_agg_income_data(dat_income_per_cap,2000,2005)
#dat_income_per_cap_2005 <- gen_agg_income_data(dat_income_per_cap,2005,2010)
#dat_income_per_cap_2010 <- gen_agg_income_data(dat_income_per_cap,2010,2015)
#dat_income_per_cap_2015 <- gen_agg_income_data(dat_income_per_cap,2015,2020)
#dat_income_per_cap_agg <- rbind(dat_income_per_cap_2000,dat_income_per_cap_2005,dat_income_per_cap_2010,dat_income_per_cap_2015)

#dat_demo_2000 <- gen_agg_demo_data(dat_demo,2000,2005)
#dat_demo_2005 <- gen_agg_demo_data(dat_demo,2005,2010)
#dat_demo_2010 <- gen_agg_demo_data(dat_demo,2010,2015)
#dat_demo_2015 <- gen_agg_demo_data(dat_demo,2015,2020)
#dat_demo_agg <- rbind(dat_demo_2000,dat_demo_2005,dat_demo_2010,dat_demo_2015)

#dat_unemploy_2000 <- gen_agg_unemploy_data(dat_unemploy,2000,2005)
#dat_unemploy_2005 <- gen_agg_unemploy_data(dat_unemploy,2005,2010)
#dat_unemploy_2010 <- gen_agg_unemploy_data(dat_unemploy,2010,2015)
#dat_unemploy_2015 <- gen_agg_unemploy_data(dat_unemploy,2015,2020)
#dat_unemploy_agg <- rbind(dat_unemploy_2000,dat_unemploy_2005,dat_unemploy_2010,dat_unemploy_2015)
#summary(dat_unemploy_agg)

# Armed Conflicts
dat_armedc_2000 <- gen_agg_armed_conflict_data(dat_armedc, 2000, 2005)
dat_armedc_2005 <- gen_agg_armed_conflict_data(dat_armedc, 2005, 2010)
dat_armedc_2010 <- gen_agg_armed_conflict_data(dat_armedc, 2010, 2015)
dat_armedc_2015 <- gen_agg_armed_conflict_data(dat_armedc, 2015, 2020)
dat_armedc_agg <- rbind(dat_armedc_2000,dat_armedc_2005,dat_armedc_2010,dat_armedc_2015)

#visa data
dat_visa_2000 <- gen_agg_visa_data(dat_visa,2000,2005)
dat_visa_2005<- gen_agg_visa_data(dat_visa,2005,2010)
dat_visa_2010 <- gen_agg_visa_data(dat_visa,2010,22015)
dat_visa_agg <- rbind(dat_visa_2000,dat_visa_2005,dat_visa_2010)
unique(dat_visa_agg$visa_required)

dat_agriemploy_2000 <- gen_agg_agri_employ_data(dat_agriemploy,2000,2005)
dat_agriemploy_2005 <- gen_agg_agri_employ_data(dat_agriemploy,2005,2010)
dat_agriemploy_2010 <- gen_agg_agri_employ_data(dat_agriemploy,2010,2015)
dat_agriemploy_2015 <- gen_agg_agri_employ_data(dat_agriemploy,2015,2020)
dat_agriemploy_agg  <- rbind(dat_agriemploy_2000,dat_agriemploy_2005,dat_agriemploy_2010,dat_agriemploy_2015)
#unique(dat_agriemploy_agg$orig)


############################ MERGE DATASETS ############################

dat_migflow$Key <- paste(dat_migflow$dest,dat_migflow$orig,dat_migflow$year0,sep="")

dat_dist$Key <- paste(dat_dist$iso_d,dat_dist$iso_o,dat_dist$year,sep="")
dat_dist <- dat_dist%>%select(Key, contig, dist, comlang_ethno)

dat_migflow_agg <- merge(x = dat_migflow,
      y = dat_dist,
      by = "Key",
      all.x = TRUE)

dat_migflow_agg <- merge(x = dat_migflow_agg,
                      y = dat_em_climate_disasters_agg,
                      by.x = c("orig","year0"),
                      by.y = c("ISO","year0"),
                      all.x = TRUE)

dat_migflow_agg <- merge(x = dat_migflow_agg,
                      y = dat_temp_agg,
                      by.x = c("orig","year0"),
                      by.y = c("ISO3","year0"),
                      all.x = TRUE)

dat_migflow_agg <- merge(x = dat_migflow_agg,
                      y = dat_pre_agg,
                      by.x = c("orig","year0"),
                      by.y = c("ISO3","year0"),
                      all.x = TRUE)

dat_migflow_agg <- merge(x = dat_migflow_agg,
                      y = dat_armedc_agg,
                      by.x = c("orig","year0"),
                      by.y = c("ISO3","year0"),
                      all.x = TRUE)

dat_migflow_agg <- merge(x = dat_migflow_agg,
                         y = dat_incomecateg,
                         by.x = c("orig"),
                         by.y = c("ISO3"),
                         all.x = TRUE)

dat_migflow_agg <- merge(x = dat_migflow_agg,
                         y = dat_gdp_per_cap_agg,
                         by.x = c("orig","year0"),
                         by.y = c("ISO3","year0"),
                         all.x = TRUE)

dat_migflow_agg <- merge(x = dat_migflow_agg,
                         y = dat_gdp_per_cap_agg,
                         by.x = c("dest","year0"),
                         by.y = c("ISO3","year0"),
                         all.x = TRUE)

#dat_migflow_agg <- merge(x = dat_migflow_agg,
#                         y = dat_demo_agg,
#                         by.x = c("orig","year0"),
#                         by.y = c("Code","year0"),
#                         all.x = TRUE)

#dat_migflow_agg <- merge(x = dat_migflow_agg,
#                        y = dat_income_per_cap_agg,
#                         by.x = c("orig","year0"),
#                         by.y = c("ISO3","year0"),
#                         all.x = TRUE)

#dat_migflow_agg <- merge(x = dat_migflow_agg,
#                         y = dat_income_per_cap_agg,
#                         by.x = c("dest","year0"),
#                         by.y = c("ISO3","year0"),
#                         all.x = TRUE)

#dat_migflow_agg <- merge(x = dat_migflow_agg,
#                         y = dat_unemploy_agg,
#                         by.x = c("orig","year0"),
#                         by.y = c("iso3c","year0"),
#                         all.x = TRUE)

dat_migflow_merged <- dat_migflow_agg
dat_migflow_merged <- dat_migflow_merged%>%rename(avg_gdp_per_cap_o = avg_gdp_per_cap.x,
                                                  avg_gdp_per_cap_d=avg_gdp_per_cap.y)

names(dat_migflow_merged) <- sub("\\.x$", "_temp", names(dat_migflow_merged))
names(dat_migflow_merged) <- sub("\\.y$", "_pre", names(dat_migflow_merged))

dat_migflow_merged$disaster_count[is.na(dat_migflow_merged$disaster_count)] <- 0
dat_migflow_merged$armed_conflicts_count[is.na(dat_migflow_merged$armed_conflicts_count)] <- 0

dat_migflow_merged$orig_continent <- countrycode(dat_migflow_merged$orig,origin = "iso3c", "continent")
dat_migflow_merged$dest_continent <- countrycode(dat_migflow_merged$dest,origin = "iso3c", "continent")

summary(dat_migflow_merged)

dat_migflow_merged_nas <- dat_migflow_merged%>%filter(is.na(dist))%>%select(orig,dest,dist)

dat_migflow_merged_nas <- merge(x = dat_migflow_merged_nas,
                         y = cities,
                         by.x = "orig",
                         by.y = "iso3",
                          all.x = TRUE)
dat_migflow_merged_nas <- merge(x = dat_migflow_merged_nas,
                                y = cities,
                                by.x = "dest",
                                by.y = "iso3",
                              all.x = TRUE)


for(i in 1:length(dat_migflow_merged_nas$dist)){
  #compute dist in km
  dat_migflow_merged_nas[i,"dist"] <- distm(c(dat_migflow_merged_nas[i,"lng.x"],dat_migflow_merged_nas[i,"lat.x"]),c(dat_migflow_merged_nas[i,"lng.y"],dat_migflow_merged_nas[i,"lat.y"]),
                                            fun=distHaversine)/1000
  print(i)
}

dat_migflow_merged_nas$Key <- paste(dat_migflow_merged_nas$dest,dat_migflow_merged_nas$orig,sep="")

dat_migflow_merged <- merge(x = dat_migflow_merged,
                                y = dat_migflow_merged_nas%>%select(Key, dist),
                                by.x = "Key",
                                by.y = "Key",
                                all.x = TRUE)

dat_migflow_merged <- dat_migflow_merged%>%mutate(dist.x = ifelse(is.na(dist.x),dist.y,dist.x))%>%select(-dist.y)%>%rename(dist = dist.x)

dat_migflow_merged <- na.omit(dat_migflow_merged)
dat_migflow_merged <- dat_migflow_merged%>%group_by(orig, dest)%>%filter(n() == 4)%>%ungroup
#wage diff
summary(dat_migflow_merged)
dat_migflow_merged$wage_diff <- dat_migflow_merged$avg_gdp_per_cap_d/dat_migflow_merged$avg_gdp_per_cap_o
#income_diff
#dat_migflow_merged$income_diff <- dat_migflow_merged$avg_income_per_cap_d/dat_migflow_merged$avg_income_per_cap_o

#climate anomalies
dat_migflow_merged <- dat_migflow_merged%>%mutate(temp_anom = (ANN_5Y_temp - Mean_LT_temp)/Sd_LT_temp, pre_anom = (ANN_5Y_pre-Mean_LT_pre)/Sd_LT_pre)
dat_migflow_merged <- dat_migflow_merged%>%mutate(temp_dev = (ANN_5Y_temp - Mean_LT_temp), pre_dev = (ANN_5Y_pre-Mean_LT_pre))
colnames(dat_migflow_merged)
dat_migflow_merged <- dat_migflow_merged%>%select(-Key,-ANN_5Y_temp,-ANN_5Y_pre,-Mean_LT_temp,-Mean_LT_pre,-Sd_LT_temp,-Sd_LT_pre,-avg_gdp_per_cap_o,-avg_gdp_per_cap_d)

dat_migflow_merged$orig <- as.character(dat_migflow_merged$orig)
dat_migflow_merged$dest <- as.character(dat_migflow_merged$dest)
dat_migflow_merged <- dat_migflow_merged %>% filter(dest != orig)

#filter_balanced <- unique(dat_migflow_merged$orig)
#dat_migflow_merged <- dat_migflow_merged%>%filter(dest %in% filter_balanced)

#summary(dat_migflow_merged)
#unique(dat_migflow_merged$orig)

#Create Agrcultural modelling data
dat_migflow_merged_agri <- merge(dat_migflow_merged,dat_agriemploy_agg,by=c("orig","year0"),all.x = TRUE)
dat_migflow_merged_agri <- na.omit(dat_migflow_merged_agri)
dat_migflow_merged_agri <- dat_migflow_merged_agri%>%group_by(orig, dest)%>%filter(n() == 4)%>%ungroup

#summary(dat_migflow_merged_agri$avg_agri_employ)

quantile(dat_migflow_merged_agri$avg_agri_employ)
dat_migflow_merged_agri$avg_agri_employ_class <- ifelse(dat_migflow_merged_agri$avg_agri_employ<5,"Low employ",dat_migflow_merged_agri$avg_agri_employ)
dat_migflow_merged_agri$avg_agri_employ_class <- ifelse(dat_migflow_merged_agri$avg_agri_employ<=40 & dat_migflow_merged_agri$avg_agri_employ>5 ,"Mid employ",dat_migflow_merged_agri$avg_agri_employ_class)
dat_migflow_merged_agri$avg_agri_employ_class <- ifelse(dat_migflow_merged_agri$avg_agri_employ>40 ,"High employ",dat_migflow_merged_agri$avg_agri_employ_class)
