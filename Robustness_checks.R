#Robustness check - continents
dat_visa_agg$Key <- paste(dat_visa_agg$dest,dat_visa_agg$orig,dat_visa_agg$year0)
dat_visa_agg <- dat_visa_agg%>%filter(visa_required != 2)
global_model_data$Key <- paste(global_model_data$dest,global_model_data$orig,global_model_data$year0)
dat_robcheck_global <- merge(dat_visa_agg,global_model_data,on="Key")
dat_robcheck_global <- dat_robcheck_global%>%group_by(orig, dest)%>%filter(n() == 3)%>%ungroup

unique(dat_visa_agg$visa_required)

dat_robcheck_europe <- dat_robcheck_global%>%filter(dest %in% european_union)
dat_robcheck_eu25 <- dat_robcheck_europe%>%filter(orig %in% european_union)
dat_robcheck_asia <- dat_robcheck_europe%>%filter(orig_continent == "Asia")
dat_robcheck_africa <- dat_robcheck_europe%>%filter(orig_continent == "Africa")
dat_robcheck_americas <- dat_robcheck_europe%>%filter(orig_continent == "Americas")

dat_robcheck_north <- dat_robcheck_europe%>%filter(income_group == "High income")
dat_robcheck_south <- dat_robcheck_europe%>%filter(income_group != "High income")

#region
unique(dat_robcheck_global$orig)
rob_global <- fit_feglm_robcheck_visa(dat_robcheck_global)
rob_global_novisa<-fit_feglm(dat_robcheck_global)

rob_europe <- fit_feglm_robcheck_visa(dat_robcheck_europe)
rob_europe_novisa<-fit_feglm(dat_robcheck_europe)

rob_eu25 <- fit_feglm_robcheck_visa(dat_robcheck_eu25)
rob_eu25_novisa<-fit_feglm(dat_robcheck_eu25)

rob_asia<-fit_feglm_robcheck_visa(dat_robcheck_asia)
rob_asia_novisa<-fit_feglm(dat_robcheck_asia)

rob_africa<-fit_feglm_robcheck_visa(dat_robcheck_africa)
rob_africa_novisa<-fit_feglm(dat_robcheck_africa)

rob_americas<-fit_feglm_robcheck_visa(dat_robcheck_americas)
rob_americas_novisa<-fit_feglm(dat_robcheck_americas)

#income
rob_high<-fit_feglm_robcheck_visa(dat_robcheck_high)
rob_high_novisa<-fit_feglm(dat_robcheck_high)

rob_low<-fit_feglm_robcheck_visa(dat_robcheck_low)
rob_low_novisa<-fit_feglm(dat_robcheck_low)

rob_upperm<-fit_feglm_robcheck_visa(dat_robcheck_upperm)
rob_upperm_novisa<-fit_feglm(dat_robcheck_upperm)

rob_lowerm<-fit_feglm_robcheck_visa(dat_robcheck_lowerm)
rob_lowerm_novisa<-fit_feglm(dat_robcheck_lowerm)


#check return migration
flow_df_robcheck <- rbind(melt(flow_2000)%>%mutate(year0  = 2000),
                  melt(flow_2005)%>%mutate(year0 = 2005),
                  melt(flow_2010)%>%mutate(year0 = 2010),
                  melt(flow_2015)%>%mutate(year0 = 2015))
colnames(flow_df_robcheck)[3] <- "mig_flow_sd"
 
flow_df_neg <- flow_df_robcheck%>%filter(mig_flow_sd<0)
flow_df_neg$mig_flow_sd <- flow_df_neg$mig_flow_sd * (-1)
 
colnames(flow_df_neg)[1] <- "orig"
colnames(flow_df_neg)[2] <- "dest"
colnames(flow_df_neg)[3] <- "return_flow"
 
flow_df_neg$Key <- paste(flow_df_neg$dest,flow_df_neg$orig,flow_df_neg$year0)
flow_df_robcheck$Key <- paste(flow_df_robcheck$dest,flow_df_robcheck$orig,flow_df_robcheck$year0) 
flow_df_robcheck<-merge(flow_df_robcheck,flow_df_neg%>%select(Key,return_flow),by="Key",all.x = TRUE)
flow_df_robcheck$return_flow <- ifelse(is.na(flow_df_robcheck$return_flow),0,flow_df_robcheck$return_flow)
flow_df_robcheck$mig_flow_sd <- flow_df_robcheck$mig_flow_sd + flow_df_robcheck$return_flow
flow_df_robcheck <- flow_df_robcheck%>%select(-return_flow)%>%mutate(mig_flow_sd = ifelse(mig_flow_sd<0,0,mig_flow_sd))%>%rename(flow_robcheck=mig_flow_sd)
 
dat_migflow_merged$Key <- paste(paste(dat_migflow_merged$dest,dat_migflow_merged$orig,dat_migflow_merged$year0))
dat_migflow_merged_robcheck <- merge(dat_migflow_merged,flow_df_robcheck%>%select(Key, flow_robcheck),all.x=TRUE)
dat_migflow_merged_robcheck$mrate_robcheck <- dat_migflow_merged_robcheck$flow_robcheck/dat_migflow_merged_robcheck$avg_pop_o
colnames(dat_migflow_merged_robcheck)
 
#Global to Global
model_global_to_europe_robcheck_mrate <- fit_feglm_robcheck_mrate(dat_migflow_merged_robcheck)
summary(model_global_to_europe_robcheck_mrate)
wald(model_global_to_europe_robcheck_mrate,vcov="HC1")

#Global to Europe
model_global_to_europe_robcheck_mrate <- fit_feglm_robcheck_mrate(dat_migflow_merged_robcheck%>%filter(dest%in% european_union))
summary(model_global_to_europe_robcheck_mrate)
wald(model_global_to_europe_robcheck_mrate,vcov="HC1")

# Europe to Europe
rob_euro_mrate <- dat_migflow_merged_robcheck%>%filter(orig %in%european_union,dest %in% european_union)
model_europe_to_europe_robcheck_mrate <- fit_feglm_robcheck_mrate(rob_euro_mrate)
summary(model_europe_to_europe_robcheck_mrate)
wald(model_europe_to_europe,vcov="HC1")
 
#Asia to EU25
asia_model_data_robcheck <- dat_migflow_merged_robcheck%>%filter(dest %in% european_union,orig_continent=="Asia")
model_asia_to_europe_robcheck_mrate <- fit_feglm_robcheck_mrate(asia_model_data_robcheck)
summary(model_asia_to_europe_robcheck_mrate)
wald(model_asia_to_europe_robcheck_mrate,vcov="HC1")

# Africa to EU25
africa_model_data_robcheck <- dat_migflow_merged_robcheck%>%filter(dest %in% european_union,orig_continent=="Africa")
model_africa_to_europe_robcheck_mrate <- fit_feglm_robcheck_mrate(africa_model_data_robcheck)
summary(model_africa_to_europe_robcheck_mrate)
wald(model_africa_to_europe_robcheck_mrate,vcov="HC1")
 
# Americas to EU25
america_model_data_robcheck <- dat_migflow_merged_robcheck%>%filter(dest %in% european_union,orig_continent=="Americas")
model_america_to_europe_robcheck_mrate <- fit_feglm_robcheck_mrate(america_model_data)
summary(model_america_to_europe_robcheck_mrate)
wald(model_america_to_europe_robcheck_mrate,vcov="HC1")

# Global north to EU27
global_north_model_data_robcheck <- dat_migflow_merged_robcheck%>%filter(dest %in% european_union,income_group =="High income")
model_global_north_to_europe_robcheck_mrate <- fit_feglm_robcheck_mrate(global_north_model_data_robcheck)
summary(model_global_north_to_europe_robcheck_mrate)
wald(model_america_to_europe_robcheck_mrate,vcov="HC1")

# Global south to EU27
global_south_model_data_robcheck <- dat_migflow_merged_robcheck%>%filter(dest %in% european_union,income_group !="High income")
model_global_south_to_europe_robcheck_mrate <- fit_feglm_robcheck_mrate(global_south_model_data_robcheck)
summary(model_global_south_to_europe_robcheck_mrate)
wald(model_america_to_europe_robcheck_mrate,vcov="HC1")


# print(europe_model_data%>%group_by(orig,year0)%>%count(armed_conflicts_count),n=120)

# endogenity
#model_global_to_global_data<-global_model_data[obs(model_global_to_global),]
#residuals <- residuals(model_global_to_global)
#fitted <- fitted(model_global_to_global)
#?wald
#plot(fitted, residuals, xlab = "fitted", ylab = "Residuals",
#     main = "Residuals vs. Potentially Endogenous Variable")
#abline(h = 0, col = "red") 


#plot(model_global_to_global_data$mstock, residuals, xlab = "mstock", ylab = "Residuals",
#     main = "Residuals vs. Potentially Endogenous Variable")
#abline(h = 0, col = "red") 
#cor(model_global_to_global_data$mstock,residuals)


#fe_model_without_endog<-fit_feglm_wostock(global_model_data)
#residuals_fe_without_endog <- residuals(fe_model_without_endog)