#install.packages("leaflet")
#install.packages("eurostat")

#library(migest)
#library(circlize)
#library(leaflet)
#library(scales)
#library(cowplot)
#library(ggthemes)
nrow(dat_migflow_merged)

source("C:/Users/gilbe/Desktop/Master Thesis/R/Modelling.R")

path_plot_analysis <- "C:/Users/gilbe/Desktop/Master Thesis/R/R_outputs/"
path_plot_latex <- "C:/Users/gilbe/Desktop/Master Thesis/Latex/thesis_template-data-science/thesis_template/"
width_pixels <- 1000
height_pixels <- 600


max(dat_migflow_merged$mig_flow_sd)
dat_migflow_merged%>%select(dest,orig,mig_flow_sd,year0)%>%filter(mig_flow_sd == 1626865)

print_sample_statistics(dat_migflow_merged$mrate)
print_sample_statistics(dat_migflow_merged$mig_flow_sd)
print_sample_statistics(dat_migflow_merged$mstock)
print_sample_statistics(dat_migflow_merged$temp_anom)
print_sample_statistics(dat_migflow_merged$pre_anom)
print_sample_statistics(dat_migflow_merged$disaster_count)
print_sample_statistics(dat_migflow_merged$wage_diff)
print_sample_statistics(dat_migflow_merged$armed_conflicts_count)

#173 origins
length(unique(dat_migflow_merged$orig))
#184 destinations
length(unique(dat_migflow_merged$dest))

### ZERO INFLATION - EUROPE
mean(
return_zero_share(dat_migflow_merged,2000),
return_zero_share(dat_migflow_merged,2005),
return_zero_share(dat_migflow_merged,2010),
return_zero_share(dat_migflow_merged,2015))
summary(dat_migflow_merged)

# Aggregate zero migration flows by regions


europe_zero_shares_plot1<-plot_map_europe_zero_flows(dat_migflow_merged, dest, dest_continent, 2000)
europe_zero_shares_plot2<-plot_map_europe_zero_flows(dat_migflow_merged, dest, dest_continent, 2005)
europe_zero_shares_plot3<-plot_map_europe_zero_flows(dat_migflow_merged, dest, dest_continent, 2010)
europe_zero_shares_plot4<-plot_map_europe_zero_flows(dat_migflow_merged, dest, dest_continent, 2015)

  
europe_zero_share_plot<-grid.arrange(
  europe_zero_shares_plot1,
  europe_zero_shares_plot2,
  europe_zero_shares_plot3,
  europe_zero_shares_plot4,
ncol = 2)

europe_flows_plot<-grid.arrange(
  plot_map_europe_flow(dat_migflow_merged, dest,year0,dest_continent,2000),
  plot_map_europe_flow(dat_migflow_merged, dest,year0,dest_continent,2005),
  plot_map_europe_flow(dat_migflow_merged, dest,year0,dest_continent,2010),
  plot_map_europe_flow(dat_migflow_merged, dest,year0,dest_continent,2015),
  ncol=2)

plot_map_europe_flow(dat_migflow_merged, dest,year0,dest_continent,2015)

europe_stocks_plot<-grid.arrange(
plot_map_europe_stock(dat_migflow_merged, dest,year0,dest_continent,2000),
plot_map_europe_stock(dat_migflow_merged, dest,year0,dest_continent,2005),
plot_map_europe_stock(dat_migflow_merged, dest,year0,dest_continent,2010),
plot_map_europe_stock(dat_migflow_merged, dest,year0,dest_continent,2015),
ncol=2)



ggsave(filename=paste(path_plot_latex,"europe","_zero",".png",sep = ""),plot=europe_zero_share_plot, device = "png",
       width = 13, height = 10, units = "in")
ggsave(filename=paste(path_plot_latex,"europe","_stock",".png",sep = ""),plot=europe_stocks_plot, device = "png",
       width = 13, height = 10, units = "in")
ggsave(filename=paste(path_plot_latex,"europe","_flow",".png",sep = ""),plot=europe_flows_plot, device = "png",
       width = 13, height = 10, units = "in")

ggsave(filename=paste(path_plot_analysis,"europe","_zero",".png",sep = ""),plot=europe_zero_share_plot, device = "png",
       width = 13, height = 10, units = "in")
ggsave(filename=paste(path_plot_analysis,"europe","_stock",".png",sep = ""),plot=europe_stocks_plot, device = "png",
       width = 13, height = 10, units = "in")
ggsave(filename=paste(path_plot_analysis,"europe","_flow",".png",sep = ""),plot=europe_flows_plot, device = "png",
       width = 13, height = 10, units = "in")

dat_migflow_merged_plot <- dat_migflow_merged
dat_migflow_merged_plot$armed_conflicts_count <- dat_migflow_merged_plot$armed_conflicts_count*1000


df_2000 <- dat_migflow_merged_plot%>%filter(year0==2000)%>%select(orig,temp_anom, pre_anom,wage_diff,armed_conflicts_count,disaster_count)%>%group_by(orig)%>%summarize(temp_anom=mean(temp_anom),
                                                                                                                                                                   pre_anom=mean(pre_anom),
                                                                                                                                                                   disaster_count = sum(disaster_count),
                                                                                                                                                                   armed_conflicts = sum(armed_conflicts_count))                                                                                                                                                                                                                                               #avg_gdp_per_cap_o =mean(avg_gdp_per_cap_o),                                                                                                                    #avg_gdp_per_cap_d=mean(avg_gdp_per_cap_d))


df_2005 <- dat_migflow_merged_plot%>%filter(year0==2005)%>%select(orig,temp_anom, pre_anom,wage_diff,armed_conflicts_count,disaster_count)%>%group_by(orig)%>%summarize(temp_anom=mean(temp_anom),
                                                                                                                                                                   pre_anom=mean(pre_anom),
                                                                                                                                                                   disaster_count = sum(disaster_count),
                                                                                                                                                                   armed_conflicts = sum(armed_conflicts_count))                                                                                                                                                                                                                                               #avg_gdp_per_cap_o =mean(avg_gdp_per_cap_o),                                                                                                                    #avg_gdp_per_cap_d=mean(avg_gdp_per_cap_d))



df_2010 <- dat_migflow_merged_plot%>%filter(year0==2010)%>%select(orig,temp_anom, pre_anom,wage_diff,armed_conflicts_count,disaster_count)%>%group_by(orig)%>%summarize(temp_anom=mean(temp_anom),
                                                                                                                                                                   pre_anom=mean(pre_anom),
                                                                                                                                                                   disaster_count = sum(disaster_count),
                                                                                                                                                                   armed_conflicts = sum(armed_conflicts_count))                                                                                                                                                                                                                                               #avg_gdp_per_cap_o =mean(avg_gdp_per_cap_o),                                                                                                                    #avg_gdp_per_cap_d=mean(avg_gdp_per_cap_d))



df_2015 <- dat_migflow_merged_plot%>%filter(year0==2015)%>%select(orig,temp_anom, pre_anom,wage_diff,armed_conflicts_count,disaster_count)%>%group_by(orig)%>%summarize(temp_anom=mean(temp_anom),
                                                                                                                         pre_anom=mean(pre_anom),
                                                                                                                         disaster_count = sum(disaster_count),
                                                                                                                         armed_conflicts = sum(armed_conflicts_count))                                                                                                                                                                                                                                               #avg_gdp_per_cap_o =mean(avg_gdp_per_cap_o),                                                                                                                    #avg_gdp_per_cap_d=mean(avg_gdp_per_cap_d))

max(df_2000$armed_conflicts)
max(df_2005$armed_conflicts)
max(df_2010$armed_conflicts)
max(df_2015$armed_conflicts)

max(df_2000$disaster_count)
max(df_2005$disaster_count)
max(df_2010$disaster_count)
max(df_2015$disaster_count)

map_pre<-plot_global_map(df_2000,df_2005,df_2010,df_2015,pre_anom,-2,3,"Pre. anom.","Average global precipitation anomalies from")
map_temp<-plot_global_map(df_2000,df_2005,df_2010,df_2015, temp_anom,-2,7,"Temp. anom.","Average global temperature anomalies from")
map_natd<-plot_global_map(df_2000,df_2005,df_2010,df_2015, disaster_count,0,25000, "Disaster count","Average global number of natural disasters per year from")
map_armedc<-plot_global_map(df_2000,df_2005,df_2010,df_2015, armed_conflicts,0,9000,"Conflicts count", "Average global armed conflicts per year from")

dat_migflow_merged_plot <- dat_migflow_merged_plot%>%mutate(NorthSouthIndicator = ifelse(income_group == "High income",1,0))
unique(dat_migflow_merged_plot$NorthSouthIndicator)

plot_global_map_gs(dat_migflow_merged_plot,NorthSouthIndicator,0,1,"Global North","Global North vs. South")

ggsave(filename=paste(path_plot_latex,"Map","_pre",".png",sep = ""),plot=map_pre, device = "png",
       width = width_pixels / 100, height = height_pixels / 100, units = "in")
ggsave(filename=paste(path_plot_latex,"Map","_temp",".png",sep = ""),plot=map_temp, device = "png",
       width = width_pixels / 100, height = height_pixels / 100, units = "in")
ggsave(filename=paste(path_plot_latex,"Map","_natd",".png",sep = ""),plot=map_natd, device = "png",
       width = width_pixels / 100, height = height_pixels / 100, units = "in")
ggsave(filename=paste(path_plot_latex,"Map","_armedc",".png",sep = ""),plot=map_armedc, device = "png",
       width = width_pixels / 100, height = height_pixels / 100, units = "in")

ggsave(filename=paste(path_plot_analysis,"Map","_pre",".png",sep = ""),plot=map_pre, device = "png",
       width = width_pixels / 100, height = height_pixels / 100, units = "in")
ggsave(filename=paste(path_plot_analysis,"Map","_temp",".png",sep = ""),plot=map_temp, device = "png",
       width = width_pixels / 100, height = height_pixels / 100, units = "in")
ggsave(filename=paste(path_plot_analysis,"Map","_natd",".png",sep = ""),plot=map_natd, device = "png",
       width = width_pixels / 100, height = height_pixels / 100, units = "in")
ggsave(filename=paste(path_plot_analysis,"Map","_armedc",".png",sep = ""),plot=map_armedc, device = "png",
       width = width_pixels / 100, height = height_pixels / 100, units = "in")

model_datasets <- list(global_model_data,
                       europe_model_data,
                       asia_model_data,
                       americas_model_data,
                       africa_model_data,
                       eu25_model_data,
                       high_income_model_data,
                       low_income_model_data,
                       upperm_income_model_data,
                       lowerm_income_model_data
)

file_names <- c("global","europe","asia","americas","africa","eu27","high","low","upperm","lowerm")
plot_timeseries_boxplot(as.data.frame(europe_model_data),"mrate","Migration rate")
counter <- 1
plot_timeseries_boxplot("global","disaster_count","Disaster count")

for(i in model_datasets){
  
  file_name <- file_names[counter]
  
  plot1 <- grid.arrange(plot_timeseries_boxplot(i,"mrate","Migration rate"),
                        plot_timeseries_boxplot(i,"mstock","Network"),
                        plot_timeseries_boxplot(i,"wage_diff","Wage differential"),
                        plot_timeseries_boxplot(i,"armed_conflicts_count","Armed violation"),
                        ncol = 2)
  
  plot2 <- grid.arrange(plot_timeseries_boxplot(i,"temp_anom","Temperature anomaly"),
                        plot_timeseries_boxplot(i,"pre_anom","Precipitation anomaly"),
                        plot_timeseries_boxplot(i,"disaster_count","Disaster count"),
                        ncol = 3)
  
  ggsave(filename=paste(path_plot_analysis,file_name,"_box_other",".png",sep = ""),plot=plot1, device = "png",
         width = 8, height = 5, units = "in")
  ggsave(filename=paste(path_plot_analysis,file_name,"_box_climate",".png",sep = ""),plot=plot2, device = "png",
         width = 8, height = 5, units = "in")
  
  ggsave(filename=paste(path_plot_latex,file_name,"_box_other",".png",sep = ""),plot=plot1, device = "png",
         width = 8, height = 5, units = "in")
  ggsave(filename=paste(path_plot_latex,file_name,"_box_climate",".png",sep = ""),plot=plot2, device = "png",
         width = 8, height = 5, units = "in")
  
  counter <- counter + 1
}

#grid.arrange(
plot_mrate_scatter(global_model_data,FALSE,temp_anom,contig,"Temperature anomaly","Shared border","Migration rate vs. temperature anomalies by shared border")
#plot_mrate_scatter(global_model_data,FALSE,pre_anom,contig,"Precipitation anomaly","Shared border","Migration rate vs. precipitation anomaly by shared border"),
#plot_mrate_scatter(global_model_data,FALSE,disaster_count,contig,"Natural disaster count","Shared border","Migration rate vs. natural disasters by shared border"),

plot_mrate_scatter(global_model_data,FALSE,temp_anom,comlang_ethno,"Temperature anomaly","Shared language (9%)","Migration rate vs. temperature anomalies by common language")
#plot_mrate_scatter(global_model_data,FALSE,pre_anom,comlang_ethno,"Precipitation anomaly","Shared language (9%)","Migration rate vs. precipitation anomaly by common language"),
#plot_mrate_scatter(global_model_data,FALSE,disaster_count,comlang_ethno,"Natural disaster count","Shared language (9%)","Migration rate vs. natural disasters by common language"),
#ncol = 3)


