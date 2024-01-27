############################ MIGRATION FLOW & STOCK DATA GENERATION ############################
path_additional <- "C:/Users/gilbe/Desktop/Master Thesis/Data/Additional Factors"

print("IMPORT POPULATION DATA")
path_pop <- paste(path_additional,"/Networks/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx",sep="")

dat_pop <- read_excel(path_pop, sheet = 1)
dat_pop <- dat_pop[-c(1:11), ]
colnames(dat_pop) <- dat_pop[1, ]
dat_pop <- dat_pop[-1, ]

dat_pop <- dat_pop%>%select(`ISO3 Alpha-code`,Year,`Life Expectancy at Birth, both sexes (years)`,`Total Population, as of 1 January (thousands)`)%>%filter(Year >= 2000, Year <= 2019)
colnames(dat_pop)[1] <- "ISO3"
colnames(dat_pop)[2] <- "year0"
colnames(dat_pop)[3] <- "LifeExp_o"
colnames(dat_pop)[4] <- "Pop_o"

# population in mio.
dat_pop$Pop_o <- as.numeric(dat_pop$Pop_o)
dat_pop$Pop_o <- dat_pop$Pop_o/1000

dat_pop$LifeExp_o <- as.numeric(dat_pop$LifeExp_o)
summary(dat_pop)

dat_pop_2000 <- gen_agg_pop_data(dat_pop,2000,2005)
dat_pop_2005 <- gen_agg_pop_data(dat_pop,2005,2010)
dat_pop_2010 <- gen_agg_pop_data(dat_pop,2010,2015)
dat_pop_2015 <- gen_agg_pop_data(dat_pop,2015,2020)
dat_pop_agg <- rbind(dat_pop_2000,dat_pop_2005,dat_pop_2010,dat_pop_2015)

colnames(dat_pop_agg)[1] <- "orig"

#https://figshare.com/collections/Bilateral_international_migration_flow_estimates_for_200_countries/4470464
print("GENERATE MIGRATION STOCKS")
#dat_abel	<- read.csv("C:/Users/gilbe/Desktop/Master Thesis/Data/2022_Abel_Global/data/bilat_mig_sex.csv", header = TRUE)
#dat_abel <- dat_abel%>%group_by(year0,orig,dest)%>%summarize(sd_rev_neg  = sum(sd_rev_neg ))%>%filter(year0 >= 2000)


#https://knowledge4policy.ec.europa.eu/dataset/ds00066_en
path_stock <- paste(path_additional,"/Networks/undesa_pd_2020_ims_stock_by_sex_destination_and_origin.xlsx",sep="")
map_path <- paste(path_additional,"/Networks/ISO_MAP_UNDESA.csv",sep="")

dat_mstock <- read_excel(path_stock, sheet = 2)
dat_mstock <- dat_mstock[-c(1:8), ]
dat_mstock <- dat_mstock[,1:14 ]
colnames(dat_mstock) <- dat_mstock[1, ]
dat_mstock <- dat_mstock[-1, ]
colnames(dat_mstock)[2] <- "dest_country"
colnames(dat_mstock)[6] <- "orig_country"
dat_mstock$dest_country <- gsub("\\*","",dat_mstock$dest_country)
dat_mstock$orig_country <- gsub("\\*","",dat_mstock$orig_country)

unique(dat_mstock$dest_country)

dat_mstock$orig <- countrycode(dat_mstock$orig_country,origin = "country.name", "iso3c")
dat_mstock$dest <- countrycode(dat_mstock$dest_country,origin = "country.name", "iso3c")

dat_mstock <- dat_mstock%>%filter(!is.na(orig),!is.na(dest))
#map <- read.csv(map_path, sep="", header=TRUE)
#colnames(map)

#map <- map%>%select(X.Country,code.....ISO.alpha3)
#map$X.Country <- gsub(",","",map$X.Country)
#colnames(map)[1] <- "Country"
#colnames(map)[2] <- "Iso3"

#dat_mstock <- dat_mstock%>%filter(dest_country %in% unique(map$Country))
#dat_mstock <- dat_mstock%>%filter(orig_country %in% unique(map$Country))

#dat_mstock <- merge(dat_mstock,map,by.x="dest_country",by.y="Country")
#colnames(dat_mstock)[15] <- "dest"
#dat_mstock <- merge(dat_mstock,map,by.x="orig_country",by.y="Country")
#colnames(dat_mstock)[16] <- "orig"
dat_mstock <- dat_mstock[,8:16]

class(dat_mstock$`1990`) <- "integer"
class(dat_mstock$`1995`) <- "integer"

dat_mstock <- dat_mstock%>%pivot_longer(c(`1990`,`1995`,`2000`,`2005`,`2010`,`2015`,`2020`),names_to = "Year", values_to = "mstock")

class(dat_mstock$Year) <- "integer"
#stock in mio.
dat_mstock$mstock <- dat_mstock$mstock/1000000
dat_mstock <- distinct(dat_mstock)
#generate_stock_matrix <- function(df,year){
  
#  stock_table <- df %>%
#    filter(Year == year) %>%
#    select(dest, orig, mstock) %>%
#    pivot_wider(names_from = dest, values_from = mstock, values_fill = 0)
  
#  names_row <- stock_table%>%select(orig)
#  stock_matrix <- as.matrix(stock_table[,-1])
  
#  return(list(names_row,stock_matrix))
#}

print("GENERATE MIGRATION FLOWS")

#orig_rownames <- generate_stock_matrix(dat_mstock,2000)[[1]]
#col is dest
#row is orig
stock_matrix_2000 <- generate_stock_matrix(dat_mstock,2000)
stock_matrix_2005 <- generate_stock_matrix(dat_mstock,2005)
stock_matrix_2010 <- generate_stock_matrix(dat_mstock,2010)
stock_matrix_2015 <- generate_stock_matrix(dat_mstock,2015)
stock_matrix_2020 <- generate_stock_matrix(dat_mstock,2020)

stock_matrix_2000[rownames(stock_matrix_2000) %in% "DEU", ]
stock_matrix_2005[rownames(stock_matrix_2005) %in% "DEU", ]

#test<-ffs_diff(
# stock_matrix_2000,
#  stock_matrix_2005,
#  decrease = "zero")
#dim(test)

flow_2000 <- stock_matrix_2005 - stock_matrix_2000
#rownames(flow_2000) <- orig_rownames$orig
flow_2000[rownames(flow_2000) %in% "DEU", ]

flow_2005 <- stock_matrix_2010 - stock_matrix_2005 
flow_2010 <- stock_matrix_2015 - stock_matrix_2010
flow_2015 <- stock_matrix_2020 - stock_matrix_2015 

flow_df <- rbind(melt(flow_2000)%>%mutate(year0  = 2000),
                 melt(flow_2005)%>%mutate(year0 = 2005),
                 melt(flow_2010)%>%mutate(year0 = 2010),
                 melt(flow_2015)%>%mutate(year0 = 2015))

colnames(flow_df)[3] <- "mig_flow_sd"
flow_df$mig_flow_sd[flow_df$mig_flow_sd<0] <- 0

dat_migflow <- flow_df

# in line with Abel migration flows
#dat_abel%>%filter(dest == "USA",orig == "AFG")
#dat_migflow%>%filter(dest == "USA",orig=="AFG")

dat_mstock$lagkey <- paste(dat_mstock$orig,dat_mstock$dest,sep="")
dat_mstock <- dat_mstock %>%
  filter(Year >=1995)%>%
  group_by(lagkey) %>%
  arrange(Year) %>%
  mutate(mstock_lag = c(NA, mstock[-n()])) %>%
  ungroup()

dat_mstock <- dat_mstock%>%filter(Year>=2000,orig!=dest)
dat_migflow <- dat_migflow%>%filter(year0>=2000,orig!=dest)

dat_migflow$Key <- paste(dat_migflow$dest,dat_migflow$orig,dat_migflow$year0,sep="")
dat_mstock$Key <- paste(dat_mstock$dest,dat_mstock$orig,dat_mstock$Year,sep="")
setdiff(dat_migflow$Key,dat_mstock$Key)

dat_migflow <- merge(x = dat_migflow,
                     y = dat_mstock%>%select(Key, mstock,mstock_lag),
                     by = "Key", all.x = TRUE)

dat_migflow$mstock[is.na(dat_migflow$mstock)] <- 0
dat_migflow$mstock_lag[is.na(dat_migflow$mstock_lag)] <- 0

dat_migflow <- merge(x = dat_migflow,
                     y = dat_pop_agg,
                     by = c("orig","year0"),
                     all.x = TRUE)
dat_migflow <- merge(x = dat_migflow,
                     y = dat_pop_agg%>%select(orig,year0,avg_pop_o),
                     by.x = c("dest","year0"),
                     by.y = c("orig","year0"),
                     all.x = TRUE)
dat_migflow <- dat_migflow%>%rename(avg_pop_o = avg_pop_o.x, avg_pop_d = avg_pop_o.y)
print("COMPUTE MIGRATION RATE")
#dat_immigrants <- dat_migflow%>%group_by(dest, year0)%>%summarize(sum_immigrants_from_o_to_d = sum(mig_flow_sd))%>%ungroup()
#colnames(dat_immigrants)[1] <- "orig"
# mig. rate as the flow from orig to dest divided by population in orig minus sum of flows to orig
# share of population which decides to migrate
#dat_migflow <- merge(dat_migflow, dat_immigrants,by = c("orig","year0"),all.x = TRUE)
dat_migflow$mrate <- dat_migflow$mig_flow_sd/dat_migflow$avg_pop_o
dat_migflow$mrate[dat_migflow$mrate<0] <- 0

dat_migflow <- dat_migflow%>%select(-mstock)%>%rename(mstock=mstock_lag)
