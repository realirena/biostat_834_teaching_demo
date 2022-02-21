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


## and then take the sum of all the different types 
#annual_dah  <- annual_dah  %>% mutate(sum_hiv_dah = rowSums(.[4:14]))

qog_data <- read.csv("~/irena/biostat_834_demo/data/qog_std_ts_jan22.csv")

qog_subset <- qog_data[,c('ccode', 'cname', 'year', 'version', 'vdem_corr')]
qog_subset <- filter(qog_subset, year>1989)

qog_subset$iso <- countrycode(qog_subset$cname, origin = 'country.name', destination = 'iso3c')

## merge the qog datasets
dah_and_qog <- merge(annual_dah, qog_subset, all.x = TRUE, by.x=c("recipient_isocode","year"), 
                     by.y=c("iso", "year"))

