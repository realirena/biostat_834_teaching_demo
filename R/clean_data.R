##
library(tidyverse)
library(data.table)
library(countrycode)
dah_data <- read.csv("~/irena/biostat_834_demo/data/IHME_DAH_DATABASE_1990_2019_Y2020M04D23.csv")
## first, filter out the funding that are captured more than once in the database
dah_data <- dah_data[!dah_data$elim_ch==1,]
hiv_cols <- colnames(dah_data)[grepl("hiv", colnames(dah_data))]
dah_data <- dah_data[,c("year", "recipient_isocode", "recipient_country", hiv_cols)]
## 
dah_subset <- dah_data[, sum_hiv_dah =sum(hiv_cols), by=list(year,recipient_isocode,recipient_country)]

dah_data[ , hiv_cols] <- apply(dah_data[ , hiv_cols], 2,function(x) as.numeric(as.character(x)))
dah_subset<- dah_data %>% drop_na()

## we can sum up the annual DAH by recipient country and year: 

annual_dah <- dah_subset %>%
 group_by(year, recipient_isocode, recipient_country) %>%
  summarise(sum_hiv_dah_19 = sum(hiv_dah_19))


write.csv(annual_dah, "~/irena/biostat_834_demo/data/cleaned_annual_dah.csv", row.names = FALSE)

## load the tax data 

tax_data <- read.csv("~/irena/biostat_834_demo/data/tax_revenue_gdp.csv")
colnames(tax_data)[1:4] <- c("country", "iso3c", "ind_name", "ind_code")
## the data is in wide format, so need to chnage to long format: 

tax_reshape <- reshape2::melt(tax_data, id.vars= c("country", "iso3c", "ind_name", "ind_code"), 
                              variable.name='year', value.name = 'tax_pct_gdp')

tax_reshape<- tax_reshape %>%
  mutate_at("year", str_replace, "X", "")

## load in the GGE data: 

gge_data <- read.csv("~/irena/biostat_834_demo/data/gge_pct_gdp.csv")

colnames(gge_data)[1:4] <- c("country", "iso3c", "ind_name", "ind_code")
## the data is in wide format, so need to chnage to long format: 

gge_reshape <- reshape2::melt(gge_data, id.vars= c("country", "iso3c", "ind_name", "ind_code"), 
                              variable.name='year', value.name = 'gge_pct_gdp')

gge_reshape<- gge_reshape %>%
  mutate_at("year", str_replace, "X", "")

gge_and_tax <- merge(gge_reshape[,c(2, 5:6)], tax_reshape[,c(2, 5:6)], by=c("year", "iso3c"))


dah_w_cov <- merge(annual_dah, gge_and_tax, by.x=c("year", "recipient_isocode"), 
                     by.y=c("year", "iso3c"))
#### load  in  battle deaths 

armed_conflict <- read.csv("~/irena/biostat_834_demo/data/ucdp-brd-dyadic-211.csv")
## get only the intrastate conflict: 

armed_conflict <- armed_conflict[armed_conflict$type_of_conflict%in%c(3,4),]

armed_subset <- armed_conflict[,c("location_inc", "side_a", "year", "bd_best", "bd_low", "bd_high",
                                  "type_of_conflict")]

armed_subset$iso3c <-  countrycode(armed_subset$location_inc, origin = 'country.name', destination = 'iso3c')

armed_subset[armed_subset$location_inc=="Yemen (North Yemen)",]$iso3c <- "YEM"

sum_battle_deaths <- armed_subset %>%
  group_by(year, location_inc, iso3c) %>%
  summarize(sum_bd_best = sum(bd_best),
            sum_bd_lower=sum(bd_low),
            sum_bd_upper = sum(bd_high))

dah_w_cov <- merge(dah_w_cov, sum_battle_deaths, all.x=TRUE,
                   by.x=c("year", "recipient_isocode"), by.y=c("year", "iso3c"))

## load in the HIV data

hiv_incidence <- read.csv("~/irena/biostat_834_demo/data/new_hiv_incidence.csv")

hiv_incidence <- hiv_incidence[,!grepl('Footnote', colnames(hiv_incidence))]

names(hiv_incidence) <- gsub("X", "", names(hiv_incidence), fixed = TRUE)

hiv_incidence <- hiv_incidence[,!grepl(c('_'), colnames(hiv_incidence))]

hiv_subset <- reshape2::melt(hiv_incidence, id.vars="Country", variable.name="year", value.name="estimated_incidence")

hiv_subset$iso3c <- countrycode(hiv_subset$Country, origin = 'country.name', destination = 'iso3c')

## join with DAH data: 

hiv_and_dah <- merge(hiv_subset, dah_w_cov, by.x=c("iso3c", "year"), by.y=c("recipient_isocode", "year"))


## load the qog data 

qog_data <- read.csv("~/irena/biostat_834_demo/data/qog_std_ts_jan22.csv")

qog_subset <- qog_data[,c('ccode', 'cname', 'year', 'version', 'vdem_corr')]
qog_subset <- filter(qog_subset, year>1989)

qog_subset$iso3c <- countrycode(qog_subset$cname, origin = 'country.name', destination = 'iso3c')

## merge the qog datasets
dah_and_qog <- merge(annual_dah, qog_subset, all.x = TRUE, by=c("iso3c","year")) 

