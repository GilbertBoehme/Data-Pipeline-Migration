############################ CLIMATE DATA IMPORT ############################

#https://www.emdat.be/
print("IMPORT CLIMATE DATA")
dat_em_climate_disasters <- read_excel(paste(path_additional,"/Climate ST - Natural Disasters/EM-Dat_natural_disasters_1990-2023.xlsx",sep=""))

colnames(dat_em_climate_disasters)[26] <- "Year"
dat_em_climate_disasters <- dat_em_climate_disasters%>%group_by(ISO, Year)%>%summarize(disaster_count= n())
dat_em_climate_disasters <- dat_em_climate_disasters%>%select(ISO,Year,disaster_count)
colnames(dat_em_climate_disasters)

#https://crudata.uea.ac.uk/cru/data/hrg/#current
dat_temp <- read.csv(paste(path_additional, "/Climate LT - Temperature/long-term-temp.csv",sep=""))
dat_temp <- dat_temp%>%select(YEAR,ANN,ISO3)%>%filter(YEAR <= 2019)

dat_pre <- read.csv(paste(path_additional, "/Climate LT - Precipitation/long-term-pre.csv",sep=""))
dat_pre <- dat_pre%>%select(YEAR,ANN,ISO3)%>%filter(YEAR <= 2019)

############################ GEOGRAPHIC, ECONOMIC AND POLITICAL DATA IMPORT ############################
print("IMPORT ECONOMIC, GEOGRAPHIC AND LINGUISTIC DATA")
#http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=8

#path_dist <- paste(path_additional,"/Distance & Language/dist_cepii.xls",sep="")
#dat_dist	<- read_excel(path_dist)
#colnames(dat_dist)
#dat_dist <- dat_dist%>%select(iso_o,iso_d,contig,dist,comlang_ethno,comlang_off,colony)

Gravity_V202211<-readRDS("C:/Users/gilbe/Desktop/Master Thesis/Data/Additional Factors/Distance & Language/Gravity_rds_V202211/Gravity_V202211.rds")
dat_dist <- Gravity_V202211%>%filter(year>= 2000, year<=2019)
dat_dist <- dat_dist%>%select(year,iso3_o,iso3_d,contig,dist,comlang_ethno,comlang_off)%>%rename(iso_o=iso3_o,iso_d=iso3_d)
dat_dist <- dat_dist%>%filter(iso_o!=iso_d)

#https://simplemaps.com/data/world-cities

cities <- read.csv("C:/Users/gilbe/Desktop/Master Thesis/Data/Additional Factors/Cities/worldcities.csv")
cities <- cities%>%filter(capital == "primary" )%>%select(iso3, lat, lng)

# gdp data

#dat_unemploy <- WDI(indicator = "SL.UEM.TOTL.ZS", 
#                    start = 2000, 
#                      end = 2020, 
#                      country = as.character(unique(dat_migflow$orig)))

#colnames(dat_gdp_per_cap)[1] <- "ISO3"
#colnames(dat_gdp_per_cap)[2] <- "Year"
#colnames(dat_gdp_per_cap)[3] <- "gdp_per_cap"
#write.csv(dat_gdp_per_cap, file = "WDI_gdp_per_cap.csv", row.names = FALSE)

dat_gdp_per_cap <- WDI(indicator = "NY.ADJ.NNTY.PC.CD", 
                    start = 2000, 
                      end = 2019, 
                    country = as.character(unique(dat_migflow$orig)))

dat_gdp_per_cap <- dat_gdp_per_cap%>%select(-iso2c,-country)%>%rename(ISO3 = iso3c,gdp_per_cap=NY.ADJ.NNTY.PC.CD,Year=year)
dat_gdp_per_cap$Key <- paste(dat_gdp_per_cap$ISO3,dat_gdp_per_cap$Year,sep="")

#https://ourworldindata.org/grapher/gdp-per-capita-worldbank?time=1996
path_gdp <- "C:/Users/gilbe/Desktop/Master Thesis/Data/Additional Factors/GDP/gdp-per-capita-worldbank.csv"

dat_gdp_per_cap_add <- read.csv(path_gdp)
dat_gdp_per_cap_add$Key <- paste(dat_gdp_per_cap_add$Code, dat_gdp_per_cap_add$Year,sep="")

colnames(dat_gdp_per_cap_add)
dat_gdp_per_cap_nas <- dat_gdp_per_cap%>%filter(is.na(gdp_per_cap))

dat_gdp_per_cap_nas$Key <- paste(dat_gdp_per_cap_nas$ISO3, dat_gdp_per_cap_nas$Year,sep="")

dat_gdp_per_cap_nas <- merge(dat_gdp_per_cap_nas,
                             dat_gdp_per_cap_add%>%select(Key,`GDP.per.capita..PPP..constant.2017.international...`),
                             by = "Key",
                             all.x = TRUE)

dat_gdp_per_cap_nas <- dat_gdp_per_cap_nas%>%mutate(gdp_per_cap = `GDP.per.capita..PPP..constant.2017.international...` )

dat_gdp_per_cap <- merge(dat_gdp_per_cap,
                         dat_gdp_per_cap_nas%>%select(Key, gdp_per_cap),
                         by = "Key",
                         all.x = TRUE)

dat_gdp_per_cap <- dat_gdp_per_cap%>%mutate(gdp_per_cap.x = ifelse(is.na(gdp_per_cap.x),gdp_per_cap.y,gdp_per_cap.x))%>%select(-gdp_per_cap.y)%>%rename(gdp_per_cap=gdp_per_cap.x)

#https://wits.worldbank.org/CountryProfile/Metadata/en/Country/All
# as of Dec-29-2023 
path_wits <- paste(path_additional,"/Income category/WITSCountryProfile-Country_Indicator_ProductMetada-en.xlsx",sep="")
dat_incomecateg <- read_excel(path_wits)
dat_incomecateg <- dat_incomecateg%>%rename(ISO3 = `Country ISO3`,income_group = `Income Group`)%>%select(ISO3,income_group)

#https://data.worldbank.org/indicator/NY.ADJ.NNTY.PC.CD
#path_income<-paste(path_additional,"/GDP/API_NY.ADJ.NNTY.PC.CD_DS2_en_excel_v2_6299631.xls",sep="")
#dat_income<-read_excel(path_income)

#dat_income<-dat_income[3:nrow(dat_income),]
#colnames(dat_income) <- dat_income[1,]
#dat_income<-dat_income[2:nrow(dat_income),]

#dat_income<-dat_income%>%select(`Country Code`,`1960`:`2020`)
#dat_income <- dat_income%>%pivot_longer(`1960`:`2020`,names_to = "Year", values_to = "income_per_cap")
#dat_income_per_cap<-dat_income%>%filter(Year>=2000,Year<=2019)
#dat_income_per_cap$income_per_cap <- as.numeric(dat_income_per_cap$income_per_cap)
#colnames(dat_income_per_cap)[1]<-"ISO3"

#http://www.fc-ssc.org/en/partnership_program/south_south_countries
#path_globalsep <- "C:/Users/gilbe/Desktop/Master Thesis/Data/UN_global-south-countries-2023.csv"
#dat_globalsep <- read.csv(path_globalsep)
#dat_global_south <- dat_globalsep%>%select(country,cca3,globalSouthCountries_globalSouth)%>%filter(globalSouthCountries_globalSouth == "Yes")
#dat_global_north <- dat_globalsep%>%select(country,cca3,globalSouthCountries_globalSouth)%>%filter(globalSouthCountries_globalSouth == "No")

#Armed conflicts
#https://ucdp.uu.se/downloads/index.html#ged_global
print("IMPORT ARMED CONFLICTS DATA")
path_armedc <- paste(path_additional,"/Armed Conflicts/GEDEvent_v23_1.rds",sep="")
dat_armedc <- readRDS(path_armedc)
dat_armedc$ISO3 <- str_extract(dat_armedc$relid, "[A-Z]{3}")
dat_armedc <- dat_armedc%>%select(ISO3,year)%>%group_by(ISO3, year)%>%summarize(armed_conflicts_count= n())

#Democracy
#path <- "C:/Users/gilbe/Desktop/Master Thesis/Data/Additional Factors/Democracy/electoral-democracy-index.csv"
#dat_demo <- read.csv(path)
#dat_demo <- dat_demo%>%filter(Year>=2000,Year < 2020, Code %in% unique(dat_migflow$orig)|Code %in% unique(dat_migflow$dest))

#visa data for robustness
path_visa<-"C:/Users/gilbe/Desktop/Master Thesis/Data/Additional Factors/Visa/DEMIG VISA Database_version 1.4.xlsx"
dat_visa <- read_excel(path_visa)
colnames(dat_visa) <- dat_visa[1,]
dat_visa <- dat_visa[2:nrow(dat_visa),]
colnames(dat_visa)[3] <- "dest"
colnames(dat_visa)[7] <- "orig"

dat_visa <- dat_visa%>%select(dest,orig,`2000`:`2014`)
dat_visa <- dat_visa%>%pivot_longer(cols = `2000`:`2014`,names_to = "Year",values_to = "visa_required")%>%filter(dest!=orig)
dat_visa <- na.omit(dat_visa)
#2 if blacklisted

# AGRICULTURE DATA
path_agri1 <- "C:/Users/gilbe/Desktop/Master Thesis/Data/Additional Factors/Agriculture/API_SL.AGR.EMPL.ZS_DS2_en_csv_v2_6314846/API_SL.AGR.EMPL.ZS_DS2_en_csv_v2_6314846.csv"
#path_agri2 <- "C:/Users/gilbe/Desktop/Master Thesis/Data/Additional Factors/Agriculture/API_AG.LND.AGRI.ZS_DS2_en_csv_v2_6299921/API_AG.LND.AGRI.ZS_DS2_en_csv_v2_6299921.csv"

dat_agriemploy <- read.csv(path_agri1)
colnames(dat_agriemploy) <- dat_agriemploy[4,]
dat_agriemploy <- dat_agriemploy[5:nrow(dat_agriemploy),]
dat_agriemploy <- dat_agriemploy%>%select(`Country Code`,`2000`:`2019`)%>%rename(orig = `Country Code`)
dat_agriemploy <- dat_agriemploy%>%pivot_longer(cols = `2000`:`2019`,names_to = "Year",values_to = "agri_employ_share")

#dat_agriland <- read.csv(path_agri2)
#colnames(dat_agriland) <- dat_agriland[4,]
#dat_agriland <- dat_agriland[5:nrow(dat_agriland),]
#dat_agriland <- dat_agriland%>%select(`Country Code`,`2000`:`2019`)%>%rename(orig = `Country Code`)
#dat_agriland <- dat_agriland%>%pivot_longer(cols = `2000`:`2019`,names_to = "Year",values_to = "agri_land_share")


 
