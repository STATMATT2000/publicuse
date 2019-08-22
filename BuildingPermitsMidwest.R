library(data.table)
library(tidyverse)
library(rvest)
library(purrr)
options(scipen = 999)

url <- "https://www2.census.gov/econ/bps/Place/Midwest%20Region"
datasets <- read_html(url) %>% 
              html_nodes("a") %>% 
              html_attr("href") %>% 
              grep("a.txt",x=.,value = TRUE)

datasets.url <- paste0(url,"/",datasets)

## Set up column names and column classes
# for 41 column variables 
col_string <- "Survey_Date,State_Code,6-Digit_ID,County_Code,Census Place_Code,FIPS Place_Code,FIPS MCD_Code,Pop,CSA_Code,CBSA_Code,Footnote_Code,Central_City,Zip_Code,Region_Code,Code,Number of_Months Rep,Place_Name,1-unit_Bldgs,1-unit_Units,1-unit_Value,2-unit_Bldgs,2-units_Units,2-units_Value,3-4 units_Bldgs,3-4 units_Units,3-4 units_Value,5+ units_Bldgs,5+ units_Units,5+ units_Value,1-units rep_Bldgs,1-unit rep_Units,1-unit rep_Value,2-unit rep_Bldgs,2-units rep_Units,2-units rep_Value,3-4 units rep_Bldgs,3-4 units rep_Units,3-4 units rep_Value,5+ units rep_Bldgs,5+ units rep_Units,5+ units rep_Value"
col_string41 <- tolower(gsub(" |-", "_",gsub("\\+", "plus",str_split(col_string,",")[[1]])))
col_class41 <- c(rep("character", 7), rep("numeric",1), rep("character", 9), rep("numeric",24))
# for 38
col_string <- "Survey_Date,State_Code,6-Digit_ID,County_Code,Census Place_Code,CSA_Code,CBSA_Code,Footnote_Code,Central_City,Zip_Code,Region_Code,Code,Number of_Months Rep,Place_Name,1-unit_Bldgs,1-unit_Units,1-unit_Value,2-unit_Bldgs,2-units_Units,2-units_Value,3-4 units_Bldgs,3-4 units_Units,3-4 units_Value,5+ units_Bldgs,5+ units_Units,5+ units_Value,1-units rep_Bldgs,1-unit rep_Units,1-unit rep_Value,2-unit rep_Bldgs,2-units rep_Units,2-units rep_Value,3-4 units rep_Bldgs,3-4 units rep_Units,3-4 units rep_Value,5+ units rep_Bldgs,5+ units rep_Units,5+ units rep_Value"
col_string38 <- tolower(gsub(" |-", "_",gsub("\\+", "plus",str_split(col_string,",")[[1]])))
col_class38 <- c(rep("character", 5), rep("character", 9), rep("numeric",24))
# for 35  column variables
col_string <- "Survey_Date,State_Code,6-Digit_ID,County_Code,MSA_CMSA, PMSA_Code,Region_Code,Code,Number of_Months Rep,,Place_Name,1-unit_Bldgs,1-unit_Units,1-unit_Value,2-unit_Bldgs,2-units_Units,2-units_Value,3-4 units_Bldgs,3-4 units_Units,3-4 units_Value,5+ units_Bldgs,5+ units_Units,5+ units_Value,1-units rep_Bldgs,1-unit rep_Units,1-unit rep_Value,2-unit rep_Bldgs,2-units rep_Units,2-units rep_Value,3-4 units rep_Bldgs,3-4 units rep_Units,3-4 units rep_Value,5+ units rep_Bldgs,5+ units rep_Units,5+ units rep_Value"
col_string35 <- tolower(gsub(" |-", "_",gsub("\\+", "plus",str_split(col_string,",")[[1]])))
col_class35 <- c(rep("character", 11), rep("numeric",24))

## Set up soft pull to figure out number of columns
soft_pull <- datasets.url %>% 
  map( ~fread(.x,, nrows = 5) )
soft_pull

## Maintain informative info about what we know about the datasets
data_maint <- data.frame( file=datasets,url=as.character(datasets.url),ncols=sapply(soft_pull,FUN=ncol))

## Pull the data for datasets with similar number of columns
test41 <- data_maint %>%              # grab data
        filter(ncols==41) %>%         # filter by specific number of columns
        pull(url) %>%                 # get only column named url
        as.character() %>%            # ensure that it is stored as a character
        map( ~fread(.x,               # read the data as a list
                    colClasses = col_class41,   # ensure appropriate colClasses
                    col.names = col_string41))  # fix column names
test38 <- data_maint %>% 
        filter(ncols==38) %>% 
        pull(url) %>% 
        as.character() %>% 
        map( ~fread(.x, colClasses = col_class38,col.names=col_string38) )
test35 <- data_maint %>% 
        filter(ncols==35) %>% 
        pull(url) %>% 
        as.character() %>% 
        map( ~fread(.x, colClasses = col_class35,col.names=col_string35) )


df41 <- rbindlist(test41)
cbsa <- "48620" # Wichita
#  "28140" # Kansas City
# "16980" # Chicago
View(df41)

Citykey <- df41[survey_date=="2018",.(state_code, `6_digit_id`, county_code, place_name, cbsa_code)][cbsa_code==cbsa]



City_summary <- df41 %>%
  filter(cbsa_code == cbsa) %>%
  group_by(survey_date) %>%
  summarise(bldg1 = sum(`1_unit_bldgs`), unit1 = sum(`1_unit_units`), value1 = sum(`1_unit_value`), 
            bldg2 = sum(`2_unit_bldgs`), unit2 = sum(`2_units_units`), value2 = sum(`2_units_value`),
            bldg3_4 = sum(`3_4_units_bldgs`), unit3_4 = sum(`3_4_units_units`), value3_4 = sum(`3_4_units_value`),
            bldg5plus = sum(`5plus_units_bldgs`), unit5plus = sum(`5plus_units_units`), value5plus = sum(`5plus_units_value`)) %>% 
  mutate(costperunit_5plus = value5plus/unit5plus)
View(City_summary)

temp <- City_summary[-1] %>%  as.matrix() %>% t() 
colnames(temp) <- City_summary$survey_date
temp <- temp  %>% as_tibble() %>% mutate(var=names(City_summary)[-1] ) %>% as.data.table()
temp41 <- temp[,c("var",City_summary$survey_date), with=FALSE]
View(temp41)

df38 <- rbindlist(test38)

City_summary <- df38 %>%
  filter(state_code %in% unique(Citykey$state_code) &
         county_code %in%unique(Citykey$county_code) &
         `6_digit_id` %in% unique(Citykey$`6_digit_id`)) %>% 
  group_by(survey_date) %>%
  summarise(bldg1 = sum(`1_unit_bldgs`), unit1 = sum(`1_unit_units`), value1 = sum(`1_unit_value`), 
            bldg2 = sum(`2_unit_bldgs`), unit2 = sum(`2_units_units`), value2 = sum(`2_units_value`),
            bldg3_4 = sum(`3_4_units_bldgs`), unit3_4 = sum(`3_4_units_units`), value3_4 = sum(`3_4_units_value`),
            bldg5plus = sum(`5plus_units_bldgs`), unit5plus = sum(`5plus_units_units`), value5plus = sum(`5plus_units_value`)) %>% 
  mutate(costperunit_5plus = value5plus/unit5plus)
View(City_summary)
temp <- City_summary[-1] %>%  as.matrix() %>% t() 
colnames(temp) <- City_summary$survey_date
temp <- temp  %>% as_tibble() %>% mutate(var=names(City_summary)[-1] ) %>% as.data.table()
temp38 <- temp[,c("var",City_summary$survey_date), with=FALSE]
View(temp38)

df35 <- rbindlist(test35)
City_summary <- df35 %>%
  filter(state_code %in% unique(Citykey$state_code) &
           county_code %in%unique(Citykey$county_code)) %>%  # `6_digit_id` %in% unique(Citykey$`6_digit_id`)
  group_by(survey_date) %>%
  summarise(bldg1 = sum(`1_unit_bldgs`), unit1 = sum(`1_unit_units`), value1 = sum(`1_unit_value`), 
            bldg2 = sum(`2_unit_bldgs`), unit2 = sum(`2_units_units`), value2 = sum(`2_units_value`),
            bldg3_4 = sum(`3_4_units_bldgs`), unit3_4 = sum(`3_4_units_units`), value3_4 = sum(`3_4_units_value`),
            bldg5plus = sum(`5plus_units_bldgs`), unit5plus = sum(`5plus_units_units`), value5plus = sum(`5plus_units_value`)) %>% 
  mutate(costperunit_5plus = value5plus/unit5plus)

View(City_summary)
temp <- City_summary[-1] %>%  as.matrix() %>% t() 
colnames(temp) <- City_summary$survey_date
temp <- temp  %>% as_tibble() %>% mutate(var=names(City_summary)[-1] ) %>% as.data.table()
temp35 <- temp[,c("var",City_summary$survey_date), with=FALSE]
View(temp35)


Building_permits <- temp35 %>%
  left_join(temp38,by="var") %>% 
  left_join(temp41, by="var")

names(Building_permits) <- c("var", as.character(1980:2018))

bldg_pmt_long <- gather(Building_permits, key=year, value = value, `1980`:`2018`)


bldg_pmt_long %>% 
  filter(var=="costperunit_5plus") %>% 
  ggplot(aes(x=as.numeric(year), y=value)) + 
  geom_line()



bldg_pmt_long %>% 
  filter(var=="value1") %>% 
  ggplot(aes(x=as.numeric(year), y=value)) + 
  geom_line()

bldg_pmt_long %>% 
  filter(var=="value2") %>% 
  ggplot(aes(x=as.numeric(year), y=value)) + 
  geom_line()

bldg_pmt_long %>% 
  filter(var=="value3_4") %>% 
  ggplot(aes(x=as.numeric(year), y=value)) + 
  geom_line()

bldg_pmt_long %>% 
  filter(var=="value5plus") %>% 
  ggplot(aes(x=as.numeric(year), y=value)) + 
  geom_line()

bldg_pmt_long %>% 
  filter(var=="unit1") %>% 
  ggplot(aes(x=as.numeric(year), y=value)) + 
  geom_line()

bldg_pmt_long %>% 
  filter(var=="unit2") %>% 
  ggplot(aes(x=as.numeric(year), y=value)) + 
  geom_line()

bldg_pmt_long %>% 
  filter(var=="unit3_4") %>% 
  ggplot(aes(x=as.numeric(year), y=value)) + 
  geom_line()

bldg_pmt_long %>% 
  filter(var=="unit5plus") %>% 
  ggplot(aes(x=as.numeric(year), y=value)) + 
  geom_line()


    
 
#unique(df$cbsa_code)
df[cbsa_code=="48620"]

# # https://www2.census.gov/econ/bps/
# # https://www2.census.gov/econ/bps/Metro/
