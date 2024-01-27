#Data Cleaning
library(tidyverse)
library(readxl)
library(countrycode)
library(reshape2)
library(plm)
library(geosphere)
library(WDI)

#install.packages("WDI")
#library(WDI)

#Descriptive statistics
library(sf)
library(ggplot2)
library(rnaturalearth)
library(maps)
library(eurostat)
library(ggthemes)
library(scales)


#Modeling
#https://cran.r-project.org/web/packages/fixest/fixest.pdf
#install.packages("fixest", 
#                 repos = c(ropensci = 'https://fastverse.r-universe.dev',
#                           CRAN = 'https://cloud.r-project.org'))
library(fixest)
#install.packages("xtable")

#Generate Latex output
library(xtable)
library(gridExtra)

