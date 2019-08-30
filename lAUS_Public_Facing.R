library(data.table)
library(tidyverse)

area_type <-  fread("https://download.bls.gov/pub/time.series/la/la.area_type")

area_codes <-  fread("https://download.bls.gov/pub/time.series/la/la.area") %>% 
                    select(area_type_code, area_code, area_text) %>%
                    left_join(area_type,by="area_type_code")



measure_codes <- fread("https://download.bls.gov/pub/time.series/la/la.measure")
period_codes <- fread("https://download.bls.gov/pub/time.series/la/la.period")

msa_data <- fread("https://download.bls.gov/pub/time.series/la/la.data.60.Metro")
# division_data <- fread("https://download.bls.gov/pub/time.series/la/la.data.61.Division")
# micro_data <- fread("https://download.bls.gov/pub/time.series/la/la.data.62.Micro")
# combined_data <- fread("https://download.bls.gov/pub/time.series/la/la.data.63.Combined")
# County_data <- fread("https://download.bls.gov/pub/time.series/la/la.data.64.County")
# City_data <- fread("https://download.bls.gov/pub/time.series/la/la.data.65.City")
All_data <- fread("https://download.bls.gov/pub/time.series/la/la.data.1.CurrentS")

series_id <- fread("https://download.bls.gov/pub/time.series/la/la.series")
# grep("Wichita", series_id$series_title, value=TRUE)
series_id <- series_id %>% 
  left_join(area_codes,by=c("area_code", "area_type_code")) %>% 
  left_join(measure_codes,by="measure_code") %>% 
  filter(grepl("Wichita", area_text, ignore.case = TRUE) &
           grepl("KS", area_text, ignore.case = TRUE) & 
           area_type_code=="B")

series_ids <- series_id$series_id 

msa_data_red <- msa_data %>%
                  filter(series_id %in% series_ids) %>%
                  left_join(series_id, by="series_id") %>%
  filter(period=="M13")

msa_data_red %>% 
  filter(measure_code != 3) %>%
  ggplot(aes(x=year,y=value,colour=as.factor(measure_text))) + geom_line() 




