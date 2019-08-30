library(data.table)
library(tidyverse)

area_codes <-  fread("https://download.bls.gov/pub/time.series/sm/sm.area")
industry_codes <- fread("https://download.bls.gov/pub/time.series/sm/sm.industry")
state_codes <- fread("https://download.bls.gov/pub/time.series/sm/sm.state")
supersector_codes <- fread("https://download.bls.gov/pub/time.series/sm/sm.supersector")
datatype_codes <- fread("https://download.bls.gov/pub/time.series/sm/sm.data_type")

series_ids <- fread("https://download.bls.gov/pub/time.series/sm/sm.series")

series_ids <- series_ids %>% 
              left_join(area_codes,by="area_code") %>% 
              left_join(industry_codes,by="industry_code") %>% 
              left_join(state_codes,by="state_code") %>% 
              left_join(supersector_codes,by="supersector_code") %>% 
              left_join(datatype_codes,by="data_type_code")

sa_data <- fread("https://download.bls.gov/pub/time.series/sm/sm.data.0.Current")
sa_data <- sa_data %>% left_join(series_ids,by="series_id")

my.ids <- series_ids %>% filter(state_name=="Kansas" & 
                        data_type_code==1 & 
                        grepl("wichita",area_name,ignore.case = T)) %>% pull(series_id)

result <- sa_data %>% filter( series_id %in% my.ids )

result2 <- result %>% 
  select(year, period, value, area_code, supersector_name, area_name, industry_name) %>% 
  filter(period == "M13")



result2 %>%  spread(year,value)





View(result2)

result2 %>% ggplot(aes(x=year,y=value,colour=as.factor(industry_name))) + geom_line() +
  guides(colour=FALSE)


