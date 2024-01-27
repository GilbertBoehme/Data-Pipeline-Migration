source("C:/Users/gilbe/Desktop/Master Thesis/R/Recipe.R")

# origin-country dummies alpha_i
# destination-year dummies alpha_j,t

#scale variables
#k <- 1000
#armed violation in k.
dat_migflow_merged$armed_conflicts_count <- dat_migflow_merged$armed_conflicts_count/100
#network in Mio. 
#distance in th. km
dat_migflow_merged$dist <- dat_migflow_merged$dist/100
max(dat_migflow_merged$disaster_count)

max(dat_migflow_merged$armed_conflicts_count)
max(dat_migflow_merged$mrate)
summary(dat_migflow_merged)

length(unique(dat_migflow_merged$orig))
length(unique(dat_migflow_merged$dest))
colnames(dat_migflow_merged)
dat_migflow_merged%>%filter(mrate == max(mrate))
dat_migflow_merged%>%filter(dest=="CHN", orig == "SYR")

dat_mstock%>%filter(orig=="SYR",dest=="CHN")
#excess temperatures
#
#dat_migflow_merged$pre_anom <- ifelse(dat_migflow_merged$pre_anom > 0, 0, dat_migflow_merged$pre_anom)
#precipitation shortages
#dat_migflow_merged$temp_anom <- ifelse(dat_migflow_merged$temp_anom < 0, 0, dat_migflow_merged$temp_anom)

#25 member states of european union
european_union = c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST', 'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT', 'ROU', 'SVK', 'SVN', 'ESP', 'SWE')
global_model_data <- dat_migflow_merged
cor(global_model_data$mstock,global_model_data$mrate)

length(unique(global_model_data$dest))

setdiff(c(global_model_data%>%filter(dest_continent =="Europe")%>%select(dest)%>%unique()),european_union)
length(unique(global_model_data$orig))
print(global_model_data%>%group_by(orig_continent,dest_continent)%>%summarize(maxrate = max(mrate)),n=30)
print(global_model_data%>%group_by(orig_continent,dest_continent)%>%summarize(count = n()),n=30)

# an increase of one unit in beta results in a (e^(beta)-1)100% increase in Y

# Global to Global
model_global_to_global <- fit_feglm(global_model_data)
summary(model_global_to_global)
wald(model_global_to_global,vcov="HC1")
?hausman

# Global to Europe
europe_model_data <- global_model_data%>%filter(dest %in% european_union)
#europe_model_data <- global_model_data%>%filter(dest_continent =="Europe")
length(unique(europe_model_data$dest))
cor(europe_model_data$mstock,europe_model_data$mrate)

model_global_to_europe <- fit_feglm(europe_model_data)
summary(model_global_to_europe)
wald(model_global_to_europe,vcov="HC1")

#EU25 to EU25
eu25_model_data <- global_model_data%>%filter(dest %in% european_union,orig %in% european_union)
length(unique(eu25_model_data$dest))
eu25_model_data%>%group_by(orig_continent)%>%summarize(count = n())
model_europe_to_europe <- fit_feglm(eu25_model_data)
summary(model_europe_to_europe)
wald(model_europe_to_europe,vcov="HC1")

print(europe_model_data%>%group_by(orig,year0)%>%count(armed_conflicts_count),n=120)

#Asia to Europe
asia_model_data <- global_model_data%>%filter(dest %in% european_union, orig_continent=="Asia")
length(unique(asia_model_data$dest))

model_asia_to_europe <- fit_feglm(asia_model_data)
summary(model_asia_to_europe)
wald(model_asia_to_europe,vcov="HC1")

#Africa to Europe
africa_model_data <- global_model_data%>%filter(dest %in% european_union, orig_continent=="Africa")
length(unique(africa_model_data$dest))
africa_model_data%>%filter(dest=="AUT",orig=="AGO")
intersect(unique(africa_model_data$dest),european_union)

model_africa_to_europe <- fit_feglm(africa_model_data)
summary(model_africa_to_europe)
wald(model_africa_to_europe,vcov="HC1")

#Americas to Europe
unique(global_model_data$dest_continent)
americas_model_data <- global_model_data%>%filter(dest %in% european_union, orig_continent=="Americas")

model_americas_to_europe <- fit_feglm(americas_model_data)
summary(model_americas_to_europe)
wald(model_americas_to_europe,vcov="HC1")

# countries per continent
#eu25_countries <- unique(eu25_model_data$orig)
#asia_model_data <- unique(asia_model_data$orig)
#africa_model_data <- unique(africa_model_data$orig)
#americas_model_data <- unique(americas_model_data$orig)


#Oceania to Europe
#unique(global_model_data$dest_continent)
#oceania_model_data <- global_model_data%>%filter(dest %in% european_union, orig_continent=="Oceania")

#model_oceania_to_europe <- fit_feglm(oceania_model_data)
#summary(model_oceania_to_europe)
#wald(model_oceania_to_europe,vcov="HC1")

#Global south 
#low income and lower middle income to eu27
unique(global_model_data$income_group)
global_south_model_data <- global_model_data%>%filter(income_group!="High income", dest %in% european_union)
model_global_south_to_eu27 <- fit_feglm(global_south_model_data)
summary(model_global_south_to_eu27)

#Global north 
#High income
unique(global_model_data$income_group)
global_north_model_data <- global_model_data%>%filter(income_group=="High income", dest %in% european_union)
model_global_north_to_eu27 <- fit_feglm(global_north_model_data)
summary(model_global_north_to_eu27)


#agriculture employ
agri_modelling_global <- dat_migflow_merged_agri
#agri_modelling_global$Key <- paste(agri_modelling_global$dest,agri_modelling_global$orig,agri_modelling_global$year0)
agri_modelling_global$avg_agri_employ_class <- as.factor(agri_modelling_global$avg_agri_employ_class)

#global migflows to eu conditioned on class
#flows from high, mid, low agri employ countries

agri_modelling_global_eu27 <- agri_modelling_global%>%filter(dest %in% european_union)
agri_modelling_global_high_eu27 <- agri_modelling_global%>%filter(avg_agri_employ_class =="High employ", dest %in% european_union)
agri_modelling_global_mid_eu27 <- agri_modelling_global%>%filter(avg_agri_employ_class=="Mid employ", dest %in% european_union)
agri_modelling_global_low_eu27 <- agri_modelling_global%>%filter(avg_agri_employ_class=="Low employ", dest %in% european_union)
agri_modelling_global_eu27%>%select(avg_agri_employ_class)%>%unique()

fit_feglm(agri_modelling_global_high_eu27)
fit_feglm(agri_modelling_global_mid_eu27)
fit_feglm(agri_modelling_global_low_eu27)

summary(fit_feglm_conditioned(agri_modelling_global,"avg_agri_employ_class"))
summary(fit_feglm_conditioned(agri_modelling_global_eu27,"avg_agri_employ_class"))

#fit_feglm(agri_modelling_global%>%filter(contig==1,avg_agri_employ_class=="High employ"))
fit_feglm_conditioned(agri_modelling_global%>%filter(avg_agri_employ_class == "High employ"),"contig")

#fit_feglm(agri_modelling_global%>%filter(contig==1,avg_agri_employ_class=="Mid employ"))
fit_feglm_conditioned(agri_modelling_global%>%filter(avg_agri_employ_class == "Mid employ"),"contig")

#fit_feglm(agri_modelling_global%>%filter(contig==1,avg_agri_employ_class=="Low employ"))
fit_feglm_conditioned(agri_modelling_global%>%filter(avg_agri_employ_class == "Low employ"),"contig")

#high_income_model_data$Key <- paste(high_income_model_data$dest,high_income_model_data$orig,high_income_model_data$year0)
#high_income_model_data_agri <- merge(high_income_model_data,agri_modelling_global%>%select(Key, avg_agri_employ_class),by="Key")
#agrimodel_high_income_to_europe <- fit_feglm_conditioned(high_income_model_data_agri,"avg_agri_employ_class")
#summary(agrimodel_high_income_to_europe)

#upperm_income_model_data$Key <- paste(upperm_income_model_data$dest,upperm_income_model_data$orig,upperm_income_model_data$year0)
#upperm_income_model_data_agri <- merge(upperm_income_model_data,agri_modelling_global%>%select(Key, avg_agri_employ_class),by="Key")
#agrimodel_upperm_income_to_europe <- fit_feglm_conditioned(upperm_income_model_data_agri,"avg_agri_employ_class")
#summary(agrimodel_upperm_income_to_europe)

#lowerm_income_model_data$Key <- paste(lowerm_income_model_data$dest,lowerm_income_model_data$orig,lowerm_income_model_data$year0)
#lowerm_income_model_data_agri <- merge(lowerm_income_model_data,agri_modelling_global%>%select(Key, avg_agri_employ_class),by="Key")
#agrimodel_lowerm_income_to_europe <- fit_feglm_conditioned(lowerm_income_model_data_agri,"avg_agri_employ_class")
#summary(agrimodel_lowerm_income_to_europe)

#build prediction dataset
#mstock
#wage_diff -> gdp_per_capita -> world economic outlook 2028
#temp_anom
#pre_anom
#disaster_count 
#armed_conflicts

#https://migrationresearch.com/migration-scenarios#migration-scenarios-4-scenarioshtm

